#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#include "llist.h"

LList llistnew() {
  LList llist = {.head = NULL, .tail = NULL, .length = 0};
  return llist;
}

void llistpush(LList* llist, void* ptr) {
  LListNode* node = malloc(sizeof(LListNode));
  *node = (LListNode) {.contents = ptr, .previous = llist->tail, .next = NULL};

  if (llist->length == 0) {
    llist->head = node;
    llist->tail = node;
  }

  llist->tail->next = node;
  llist->tail = node;

  llist->length++;
}

void llistpop(LList* llist) {
  LListNode* prev_tail = llist->tail;
  llist->tail = prev_tail->previous;
  free(prev_tail);

  llist->length--;
}

void* llistget(LList* llist, size_t index) {
  LListNode* current;
  if (index < llist->length / 2) {
    current = llist->head;
    for (size_t i = 0; i < index; i++)
      current = current->next;
    return current->contents;
  } else {
    current = llist->tail;
    for (size_t i = llist->length - 1; i > index; i--)
      current = current->previous;
    return current->contents;
  }
}

void llistset(LList* llist, size_t index, void* ptr) {
  LListNode* target = llistget(llist, index);
  free(target->contents);
  target->contents = ptr;
}