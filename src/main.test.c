extern int lexer_test();
extern int cst_test();
extern int parser_test(void);
extern int memcxt_test();

int main() {
    memcxt_test();
    lexer_test();
    cst_test();
    parser_test();
    return 0;
}