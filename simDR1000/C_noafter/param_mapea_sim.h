
	integer , parameter :: nSb=5

	double precision M(nSb,nSb)
	common /patmatrix/ M

	integer, parameter :: Nc = (2**nSb)-1
	double precision X(Nc,nSb)
	integer nodoboo(Nc), nodobooinv(Nc), nX(Nc), nnodos, lk(Nc)
	common /ppal/ X, nodoboo,nodobooinv, nX, nnodos, lk
