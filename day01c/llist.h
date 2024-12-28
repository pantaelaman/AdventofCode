#ifndef LLIST
#define LLIST

typedef struct LListNode {
  void* contents;
  struct LListNode* previous;
  struct LListNode* next;
} LListNode;

typedef struct {
  LListNode* head;
  LListNode* tail;
  size_t length;
} LList;

LList llistnew();
void llistpush(LList* llist, void* ptr);
void llistpop(LList* llist);
void* llistget(LList* llist, size_t index);
void llistset(LList* llist, size_t index, void* ptr);

#endif
