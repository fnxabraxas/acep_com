#include <wiki1.h>

int main(int argc, char *argv[])
{
	char nombre[200];
	
	int j, *numdir, *numinv;

	long i, m, guess, nrec, ncrec, numtran = 0, nodes;
	long **conrec, **directo, **inverso, *trans, *numrec, *visited;
	
	FILE *grafos, *net;
		
	trans = (long *) malloc(nodes*sizeof(long));
	
	sprintf(nombre, "../results/input.dat");
	net = fopen(nombre, "r");

	nodes = readnodes(net);
	fclose(net);
	net = fopen(nombre, "r");

	/* Read the adjacency matrix and weights for the graph */

	directo = (long **) malloc(nodes*sizeof(long *));
	numdir = (int *) malloc(nodes*sizeof(int));
	inverso = (long **) malloc(nodes*sizeof(long *));
	numinv = (int *) malloc(nodes*sizeof(int));
	trans = (long *) malloc(nodes*sizeof(long));

	for (i = 0; i < nodes; i++) numdir[i] = 0;

	readFA(net, directo, numdir);

	printf("Number of nodes: %ld\n", nodes);
	fclose(net);

	ncrec = 0;
	nrec = 0;
	
	conrec = (long **) malloc(nodes*sizeof(long *));
	numrec = (long *) malloc(nodes*sizeof(long));
	visited = (long *) malloc(nodes*sizeof(long));

	for (i = 0; i < nodes; i++) {

		numinv[i] = 0;
		inverso[i] = NULL;
		visited[i] = 0;
	}

	/* Recurrent set calculation */

	inversegraph(directo, numdir, inverso, numinv, nodes, visited);		/* Inverse graph */

	guess = inicapicuest(directo, numdir, inverso, numinv, nodes);		/* Initial guess */

	capicuest(directo, numdir, inverso, numinv, nodes, conrec, numrec, &ncrec, trans, &numtran, visited, guess);

	printf("Estados transitorios: %ld\n", numtran);
	printf("Conjuntos recurrentes: %ld\n", ncrec);

	sprintf(nombre, "../results/recurrentes.dat");
	grafos = fopen(nombre, "w");
	fprintf(grafos, "%s \n", "# Num    Nodes");

	for (i = 0; i < ncrec; i++) {
			fprintf(grafos, "  %ld     ", numrec[i]);
			for (j = 0; j < numrec[i]; j++) fprintf(grafos, "%ld ", conrec[i][j]+1);
			fprintf(grafos, "\n");
	}
        fclose(grafos);

	/* Garbage */

	for (i = 0; i < nodes; i++) {

		free(directo[i]);		free(inverso[i]);
	}

	for (i = 0; i < ncrec; i++) free(conrec[i]);

	free(directo);		free(numdir);
	free(inverso);		free(numinv);
	free(numrec);		free(conrec);
	free(trans);
	free(visited);

	return 0;
}
