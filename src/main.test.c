extern int lexer_test();
extern int cst_test();
extern int parser_test(void);
extern int memcxt_test();
extern int asttrans_test(void);

int main() {
    int r = 0;
    r |= memcxt_test();
    r |= lexer_test();
    r |= cst_test();
    r |= parser_test();
    if (!r)
        r |= asttrans_test();
    return r;
}