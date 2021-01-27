"""
ssNake : https://github.com/smeerten/ssnake

https://github.com/smeerten/ssnake/blob/master/src/functions.py
    def ACMEentropy(phaseIn, data, x, firstOrder=True)

https://github.com/smeerten/ssnake/blob/master/src/spectrum.py
    def autoPhase(self, phaseNum=0, axis=-1, locList=None, returnPhases=False, select=slice(None)):
    def getHyperData(self, *args):
        return self.data.getHyperData(*args)

autophasing based on ACME Entropy
http://wiki.icmc.usp.br/images/8/8c/Acme.pdf

"""

def autoPhase(self, phaseNum=0, axis=-1, locList=None, returnPhases=False, select=slice(None)):
    """
    Autophases a spectrum.
    Parameters
    ----------
    phaseNum : {0, 1}, optional
        Order up to which to perform the autophasing.
        For 0 only zero order phasing is performed, for 1 both zero and first order phasing is performed.
        0 by default.
    axis : int, optional
        The dimension.
        By default the last dimension is used.
    locList : array_like of int, optional
        The indices of the trace to determine the phase values.
        By default the first index of each dimension is used.
    returnPhases : bool, optional
        If True the determined phases are return.
        False by default.
    select : Slice, optional
        An optional selection of the spectrum data on which the phasing is performed.
        By default the entire data is used.
    Raises
    ------
    SpectrumException
        When locList does not have the same length as the number of dimensions or when locList contains invalid indices.
    """
    axis = self.checkAxis(axis)
    if locList is None:
        locList = [0]*self.ndim()
    if len(locList) != self.ndim():
        raise SpectrumException("Data does not have the correct number of dimensions")
    if np.any(locList >= np.array(self.shape())) or np.any(np.array(locList) < 0):
        raise SpectrumException("The location array contains invalid indices")
    locList = np.array(locList, dtype=object)
    locList[axis] = slice(None)
    self.data.icomplexReorder(axis)
    if self.spec[axis] == 0:
        self.__fourier(axis, tmp=True)
    tmp = self.data[locList]
    tmp = tmp.getHyperData(0)   # only optimize on the hyper real data
    x = np.fft.fftshift(np.fft.fftfreq(len(tmp), 1.0 / self.sw[axis])) / self.sw[axis]
    if phaseNum == 0:
        phases = scipy.optimize.minimize(func.ACMEentropy, [0], (tmp, x, False), method='Powell', options={'xtol': AUTOPHASETOL})
        phase0 = phases['x']
        phase1 = 0.0
    elif phaseNum == 1:
        phases = scipy.optimize.minimize(func.ACMEentropy, [0, 0], (tmp, x), method='Powell', options={'xtol': AUTOPHASETOL})
        phase0 = phases['x'][0]
        phase1 = phases['x'][1]
    if self.ref[axis] is None:
        offset = 0
    else:
        offset = self.freq[axis] - self.ref[axis]
    vector = np.exp(np.fft.fftshift(np.fft.fftfreq(self.shape()[axis], 1.0 / self.sw[axis]) + offset) / self.sw[axis] * phase1 * 1j)
    vector = vector.reshape(vector.shape + (1, )*(self.ndim()-axis-1))
    self.data[select] *= np.exp(phase0 * 1j) * vector
    if self.spec[axis] == 0:
        self.__invFourier(axis, tmp=True)
    self.data.icomplexReorder(axis)
    Message = "Autophase: phase0 = " + str(phase0 * 180 / np.pi) + " and phase1 = " + str(phase1 * 180 / np.pi) + " for dimension " + str(axis + 1)
    self.addHistory(Message)
    if returnPhases:
        if phaseNum == 0:
            return [phases['x']]
        return phases['x']
    self.redoList = []
    if not self.noUndo:
        self.undoList.append(lambda self: self.phase(-phase0, -phase1, axis))

def __phase(self, phase0, phase1, offset, axis, select=slice(None)):
    vector = np.exp(np.fft.fftshift(np.fft.fftfreq(self.shape()[axis], 1.0 / self.sw[axis]) + offset) / self.sw[axis] * phase1 * 1j)
    if self.spec[axis] == 0:
        self.__fourier(axis, tmp=True)
    vector = vector.reshape(vector.shape + (1, )*(self.ndim()-axis-1))
    self.data.icomplexReorder(axis)
    self.data[select] *= np.exp(phase0 * 1j) * vector
    self.data.icomplexReorder(axis)
    if self.spec[axis] == 0:
        self.__invFourier(axis, tmp=True)

def phase(self, phase0=0.0, phase1=0.0, axis=-1, select=slice(None)):
    """
    Phases a spectrum along a given dimension.
    Parameters
    ----------
    phase0 : float, optional
        Zero order phase.
        0.0 by default.
    phase1 : float, optional
        First order phase.
        0.0 by default.
    axis : int, optional
        The dimension.
        By default the last dimension is used.
    select : Slice, optional
        An optional selection of the spectrum data on which the phasing is performed.
        By default the entire data is used.
    """
    axis = self.checkAxis(axis)
    if self.ref[axis] is None:
        offset = 0
    else:
        offset = self.freq[axis] - self.ref[axis]
    self.__phase(phase0, phase1, offset, axis, select=select)
    Message = "Phasing: phase0 = " + str(phase0 * 180 / np.pi) + " and phase1 = " + str(phase1 * 180 / np.pi) + " for dimension " + str(axis + 1)
    if not isinstance(select, slice):
        Message = Message + " with slice " + str(select)
    elif select != slice(None, None, None):
        Message = Message + " with slice " + str(select)
    self.addHistory(Message)
    self.redoList = []
    if not self.noUndo:
        self.undoList.append(lambda self: self.phase(-phase0, -phase1, axis, select=select))



def ACMEentropy(phaseIn, data, x, firstOrder=True):
    """
    Calculates the cost value for autophasing.
    Parameters
    ----------
    phaseIn : list of float
        Should contain two values, the first one is the zero order phase, and the second one the first order phase.
    data : ndarray
        The data to be phased.
    x : ndarray
        The x-axis corresponding to the first order phasing.
    firstOrder : bool, optional
        If True, the first order phasing is included.
        True by default.
    Returns
    -------
        The cost value.
    """
    phase0 = phaseIn[0]
    if firstOrder:
        phase1 = phaseIn[1]
    else:
        phase1 = 0.0
    L = len(data)
    s0 = data * np.exp(1j * (phase0 + phase1 * x))
    s2 = np.real(s0)
    ds1 = np.abs((s2[3:L] - s2[1:L - 2]) / 2.0)
    p1 = ds1 / sum(ds1)
    p1[np.where(p1 == 0)] = 1
    h1 = -p1 * np.log(p1)
    H1 = sum(h1)
    Pfun = 0.0
    as1 = s2 - np.abs(s2)
    sumas = sum(as1)
    if np.real(sumas) < 0:
        Pfun = Pfun + sum(as1**2) / 4 / L**2
    return H1 + 1000 * Pfun