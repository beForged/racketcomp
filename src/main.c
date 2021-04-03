#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "types.h"

// in bytes
#define heap_size 1000000

int64_t entry(void *);
void print_result(int64_t);
void print_pair(int64_t);
void print_immediate(int64_t);
void print_char(int64_t);
void print_string(int64_t);
void print_string_char(int64_t);
void print_codepoint(int64_t);

int main (int argc, char** argv){
    void * heap = malloc(heap_size);
    int64_t result = entry(heap);
    print_result(result);
    printf("\n");
    free(heap);
    return 0;
}

void error() {
    printf("err");
    exit(1);
}

void internal_err(){
    printf("rts-error");
    exit(1);
}

void print_result(int64_t result) {
    switch(result_type_mask & result){
        case type_imm:
            print_immediate(result);
            break;
        case type_box:
            printf("#&");
            print_result(*((int64_t *)(result ^ type_box)));
            break;
        case type_pair:
            printf("(");
            print_pair(result);
            printf(")");
            break;
        case type_string:
            printf("\"");
            print_string(result);
            printf("\"");
            break;
        case type_proc:
            printf("procedure");
            break;
        default:
            internal_err();
    }
}

void print_immediate(int64_t a) {
    switch(imm_type_mask & a){
        case imm_type_int:
            printf("%" PRId64 , a >> imm_shift);
            break;
        case imm_type_bool:
            printf("#%c", a >> imm_shift ? 't' : 'f');
            break;
        case imm_val_empty:
            printf("()");
            break;
        case imm_type_char:
            print_char(a);
        default:
            break;
            internal_err();
    }
}

void print_pair(int64_t a) {
    int64_t car = *((int64_t *)((a + 8) ^ type_pair));
    int64_t cdr = *((int64_t *)((a + 0) ^ type_pair));
    print_result(car);
    if((imm_type_mask & cdr) == imm_val_empty) { 
        //empty
    } else if((result_type_mask & cdr) == type_pair){
        printf(" ");
        print_pair(cdr);
    } else {
        printf(" . "); 
        print_result(cdr);
    }
}


//todo impl string printing
void print_char(int64_t v) {
    int64_t c = v >> imm_shift;
    switch(c){
        default:
            print_codepoint(v);
    }
}

void print_string(int64_t v) {
    int64_t * str = (int64_t *) (v ^ type_string);
    int64_t len = (str[0] >> imm_shift);
    int i;
    for(i = 0; i < len; i++){
        print_string_char(str[i+1]);
    }
}
