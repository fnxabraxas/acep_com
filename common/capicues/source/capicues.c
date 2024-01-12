#include <wiki1.h>

void inversegraph(long **directo, int *ndir, long **inverso, int *ninv, long total, long *visited)
{
  /* El grafo inverso ha de ser solo un array de dobles punteros inicializados a NULL 
  Igualmente, ninv debe estar inicializado a 0 */
  
  long i, index;
  int j;
  
  for (i = 0; i < total; i++) {
  
    for (j = 0; j < ndir[i]; j++) {
    
      index = directo[i][j];

      if (visited[index] == 0) {

        ninv[index]++;
        inverso[index] = (long *) realloc(inverso[index], ninv[index]*sizeof(long));
        inverso[index][ninv[index]-1] = i;
      }
    }
  }

  return;
}

void mergeSort(long *numbers, long *temp, long array_size)
{
   m_sort(numbers, temp, 0, array_size-1);
  
  return;
}
 
 
void m_sort(long *numbers, long *temp, long left, long right)
{
  long mid;

  if (right > left) {

    mid = (right + left)/2;
    m_sort(numbers, temp, left, mid);
    m_sort(numbers, temp, mid+1, right);

    merge(numbers, temp, left, mid+1, right);
  }
  
  return;
}
 
void merge(long *numbers, long *temp, long left, long mid, long right)
{
  long i, left_end, num_elements, tmp_pos;
 
  left_end = mid-1;
  tmp_pos = left;
  num_elements = right-left+1;

  while ((left <= left_end) && (mid <= right)) {

    if (numbers[left] <= numbers[mid]) {

      temp[tmp_pos] = numbers[left];
      tmp_pos = tmp_pos+1;
      left = left+1;

    } else {

      temp[tmp_pos] = numbers[mid];
      tmp_pos = tmp_pos+1;
      mid = mid+1;
    }
  }

  while (left <= left_end) {

    temp[tmp_pos] = numbers[left];
    left = left+1;
    tmp_pos = tmp_pos+1;
  }

  while (mid <= right) {

    temp[tmp_pos] = numbers[mid];
    mid = mid+1;
    tmp_pos = tmp_pos+1;
  }

  for (i = 0; i <= num_elements; i++) {

    numbers[right] = temp[right];
    right = right-1;
  }
  
  return;
}

int inclusion(int *menor, int *mayor, long size)
{
  /* flag = 0 si menor no esta includo en mayor y 1 al contrario */
  
  long i, j = 0;
  int flag = 1;
  int *prod = malloc(size*sizeof(int));

  for (i = 0; i < size; i++) prod[i] = mayor[i]*menor[i];
  
  for (i = 0; i < size; i++) {
  
    if (menor[i] != prod[i]) {
    
      flag = 0;
      break;
    }
  }
  
  free(prod);

  return(flag);
}

void accesibles(long i, long **grafo, int *num, long total, int *lista, long *cardinal)
{
  /* Calcula todos los nodos accesibles a partir del nodo i. */
  
  long j, k = i;
  pila1 *elem;
  
  elem = (pila1 *) malloc(sizeof(pila1));
  
  elem->valor = -1;
  elem->siguiente = NULL;
  
  for (j = 0; j < total; j++)
    lista[j] = 0;
  
  lista[i] = 1;

  *cardinal = 1;
  
  new_value(&elem, k);
  
  do{
  
    k = off_value(&elem);
    
    for (j = 0; j < num[k]; j++){
    
      if (lista[grafo[k][j]] == 0){
      
        lista[grafo[k][j]] = 1;
        (*cardinal)++;
        new_value(&elem, grafo[k][j]);
      }
    }
    
  } while (elem->siguiente != NULL);
  
  free(elem);
  
  return;
}

void capicuest(long **directo, int *numdir, long **inverso, int *numinv, long total, 
  long **conrec, long *numrec, long *ncrec, long *trans, long *numtran, long *visited, long ini)
{
  long i = ini, j, k, numEx, numOx, numOxj, numleft = total;
  int *Ex = malloc(total*sizeof(int));
  int *Ox = malloc(total*sizeof(int));
  int *Oxj = malloc(total*sizeof(int));
  int band;
  
  while (numleft > 0) {
  
    if (visited[i] == 0) {

      band = 0;

      accesibles(i, directo, numdir, total, Ex, &numEx);
      printf("Nodos accesibles desde %ld = %ld.\n", i, numEx);

      accesibles(i, inverso, numinv, total, Ox, &numOx);
      printf("Nodos accedidos por %ld = %ld.\n", i, numOx);
      
      if (numEx <= numOx) band = inclusion(Ex, Ox, total);

      if (band == 1) { /* Ex esta incluido en Ox */

        /* Quitamos los recurrentes */

        printf("Calculado un recurrente de %ld estados.\n", numEx);

        numrec[*ncrec] = numEx;
        conrec[*ncrec] = (long *) malloc(numEx*sizeof(long));
        k = 0;
        for (j = 0; j < total; j++) {

          if (Ex[j] == 1) {

            conrec[*ncrec][k] = j;
            visited[j] = 1;
            k++;
          }
        }
        (*ncrec)++;

        /* Quitamos los transitorios */

        for (j = 0; j < total; j++) {

          if (Ox[j] == 1 && visited[j] == 0) {

            visited[j] = 1;
            trans[*numtran] = j;
            (*numtran)++;
          }
        }

        printf("Calculados %ld estados transitorios.\n", *numtran);
      }

      numleft = 0;
      for (j = 0; j < total; j++) {

        if (visited[j] == 0) numleft++;
      }
    }

    /* Calculo el siguiente i */

    if (numleft > 0) {

      j = i+1;
      while (visited[j] == 1) j++;

      i = j;
      
      if (i == total) {
      
        j = 0;
        while (visited[j] == 1) j++;

        i = j;
      }
    }
  }

  free(Ex);    free(Ox);    free(Oxj);

  return;
}

long OxfueraEx(int *Ex, int *Ox, long size, long *lista)
{
  /* Devuelve la lista de elementos de Ex que no estan en Ox */
  
  long i, sizelista = 0;

  for (i = 0; i < size; i++) {
  
    if (Ex[i] == 1 && Ox[i] == 0) {
    
      lista[sizelista] = i;
      sizelista++;
    }
  }
  
  return(sizelista);
}

long inicapicuest(long **directo, int *numdir, long **inverso, int *numinv, long total)
{
  long j, k, numEx, numOx, sizelista = 0, ini;
  int *Ex = malloc(total*sizeof(int));
  int *Ox = malloc(total*sizeof(int));
  long *inguess = malloc(total*sizeof(long));
  int band;
  
  band = 0;

  accesibles(0, directo, numdir, total, Ex, &numEx);
  printf("Nodos accesibles desde %d = %ld.\n", 0, numEx);

  accesibles(0, inverso, numinv, total, Ox, &numOx);
  printf("Nodos accedidos por %d = %ld.\n", 0, numOx);

  sizelista = OxfueraEx(Ex, Ox, total, inguess);

  if (sizelista == 0) { /* Ex esta incluido en Ox */

    ini = 0;

  } else ini = inguess[0];

  free(Ex);    free(Ox);    free(inguess);

  return(ini);
}

long readFA(FILE *net, long **adjacency, int *nfil)
{
  /* Allocation of memory for 'adjacency' is made as long as we read the file */
  
  long i, j, ini, inicp, vec[10000];

  int flag;
  
  
  flag = fscanf(net, "%ld %ld \n", &ini, &(vec[0]));

  inicp = ini;

  do {
  
    i = 1;

    while (ini == inicp && flag != EOF) {

      flag = fscanf(net, "%ld %ld \n", &ini, &(vec[i]));
      
      if (flag > 0 && ini == inicp) i++;
    }

    nfil[inicp-1] = i;
    
     
    adjacency[inicp-1] = (long *) malloc(i*sizeof(long));
    
    
    for (j = 0; j < nfil[inicp-1]; j++) {
    
      adjacency[inicp-1][j] = vec[j]-1;
    }

    vec[0] = vec[i];
    inicp = ini;
      
  } while (flag != EOF);

  return(0);
}

long readnodes(FILE *net)
{
  /* Allocation of memory for 'adjacency' is made as long as we read the file */
  
  long nodes = 0, actual1, actual2;

  int flag;
  
  do {
  
    flag = fscanf(net, "%ld %ld \n", &actual1, &actual2);
  
    if (actual1 > nodes) nodes = actual1;
    if (actual2 > nodes) nodes = actual2;

  } while(flag != EOF);

  return(nodes);
}

