extern int lexer_test();
extern int ast_test();
extern int parser_test(void);

int main() {
    lexer_test();
    ast_test();
    parser_test();
    return 0;
}