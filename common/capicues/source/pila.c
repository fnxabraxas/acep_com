#include <wiki1.h>

void new_value(pila1 **elem, long value)
{
	pila1 *nueva_pila;
	
	nueva_pila = (pila1 *)malloc(sizeof(pila1));
	
	nueva_pila->siguiente = (void *)(* elem);
	*elem = nueva_pila;
	
	(*elem)->valor = value;
	
	return;
}

long off_value(pila1 **elem)
{
	int i, value = (*elem)->valor;
	pila1 *nueva_pila;
	
	nueva_pila = *elem;
	
	*elem = nueva_pila->siguiente;
	
	free(nueva_pila);
	
	return(value);
}
