void new_value(pila1 **elem, long value);
long off_value(pila1 **elem);

void inversegraph(long **directo, int *ndir, long **inverso, int *ninv, long total, long *visited);
void mergeSort(long *numbers, long *temp, long array_size);
void m_sort(long *numbers, long *temp, long left, long right);
void merge(long *numbers, long *temp, long left, long mid, long right);
int inclusion(int *menor, int *mayor, long sizem);
void accesibles(long i, long **grafo, int *num, long total, int *lista, long *cardinal);
void capicuest(long **directo, int *numdir, long **inverso, int *numinv, long total, 
	long **conrec, long *numrec, long *ncrec, long *trans, long *numtran, long *visited, long i);
long OxfueraEx(int *Ox, int *Ex, long size, long *lista);
long inicapicuest(long **directo, int *numdir, long **inverso, int *numinv, long total);

long readFA(FILE *net, long **adjacency, int *nfil);
long readnodes(FILE *net);


