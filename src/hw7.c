//Junyi Wang
#include "hw7.h"

static const char* skip_spaces_sf(const char *p) {
    while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') {
        ++p;
    }
    return p;
}

static int read_int_sf(const char **pp) {
    const char *p = skip_spaces_sf(*pp);
    int sign = 1;
    int val = 0;

    if (*p == '+') {
        ++p;
    } else if (*p == '-') {
        sign = -1;
        ++p;
    }

    while (*p >= '0' && *p <= '9') {
        val = val * 10 + (*p - '0');
        ++p;
    }

    *pp = p;
    return sign * val;
}

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    bst_sf *node = malloc(sizeof(bst_sf));
    node->mat = mat;
    node->left_child = NULL;
    node->right_child = NULL;

    if (root == NULL) {
        return node;
    }

    bst_sf *cur = root;
    while (1) {
        if (mat->name < cur->mat->name) {
            //Insert the left subtree
            if (cur->left_child) {
                cur = cur->left_child;
            } else {
                cur->left_child = node;
                break;
            }
        } else {
            //Insert the right subtree
            if (cur->right_child) {
                cur = cur->right_child;
            } else {
                cur->right_child = node;
                break;
            }
        }
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    bst_sf *cur = root;
    while (cur) {
        if (name == cur->mat->name) {
            return cur->mat;
        } else if (name < cur->mat->name) {
            cur = cur->left_child;
        } else {
            cur = cur->right_child;
        }
    }
    return NULL;
}

void free_bst_sf(bst_sf *root) {
    if (!root) return;
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    if (root->mat) {
        free(root->mat);
    }
    free(root);
}


matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned int rows = mat1->num_rows;
    unsigned int cols = mat1->num_cols;
    matrix_sf *res = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    res->name = '?';              // temporary
    res->num_rows = rows;
    res->num_cols = cols;

    unsigned int n = rows * cols;
    for (unsigned int i = 0; i < n; ++i) {
        res->values[i] = mat1->values[i] + mat2->values[i];
    }
    return res;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned int r = mat1->num_rows;
    unsigned int k = mat1->num_cols;
    unsigned int c = mat2->num_cols;

    matrix_sf *res = malloc(sizeof(matrix_sf) + r * c * sizeof(int));
    res->name = '?';              // temporary
    res->num_rows = r;
    res->num_cols = c;

    for (unsigned int i = 0; i < r; ++i) {
        for (unsigned int j = 0; j < c; ++j) {
            long long sum = 0;
            for (unsigned int t = 0; t < k; ++t) {
                sum += (long long)mat1->values[i * k + t] *
                       (long long)mat2->values[t * c + j];
            }
            res->values[i * c + j] = (int)sum;
        }
    }
    return res;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    unsigned int r = mat->num_rows;
    unsigned int c = mat->num_cols;

    matrix_sf *res = malloc(sizeof(matrix_sf) + r * c * sizeof(int));
    res->name = '?';              // temporary
    res->num_rows = c;
    res->num_cols = r;

    for (unsigned int i = 0; i < r; ++i) {
        for (unsigned int j = 0; j < c; ++j) {
            res->values[j * r + i] = mat->values[i * c + j];
        }
    }
    return res;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    const char *p = expr;

    unsigned int rows = (unsigned int)read_int_sf(&p);
    unsigned int cols = (unsigned int)read_int_sf(&p);

    p = skip_spaces_sf(p);
    if (*p == '[') {
        ++p;
    }

    matrix_sf *m = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    m->name = name;
    m->num_rows = rows;
    m->num_cols = cols;

    for (unsigned int i = 0; i < rows; ++i) {
        for (unsigned int j = 0; j < cols; ++j) {
            int v = read_int_sf(&p);
            m->values[i * cols + j] = v;
            p = skip_spaces_sf(p);
        }
        p = skip_spaces_sf(p);
        if (*p == ';') {
            ++p;
        }
        p = skip_spaces_sf(p);
    }

    p = skip_spaces_sf(p);
    if (*p == ']') {
        ++p;
    }

    return m;
}

static int prec_sf(char op) {
    if (op == '\'') return 3;
    if (op == '*')  return 2;
    if (op == '+')  return 1;
    return 0;
}

char* infix2postfix_sf(char *infix) {
    size_t n = strlen(infix);
    char *out = malloc(n + 8);
    size_t oi = 0;

    char *stack = malloc(n + 8);
    int top = -1;

    for (size_t i = 0; i < n; ++i) {
        char c = infix[i];

        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            continue;
        }

        if (c >= 'A' && c <= 'Z') {
            out[oi++] = c;
        } else if (c == '\'') {
            out[oi++] = c;
        } else if (c == '(') {
            stack[++top] = c;
        } else if (c == ')') {
            while (top >= 0 && stack[top] != '(') {
                out[oi++] = stack[top--];
            }
            if (top >= 0 && stack[top] == '(') {
                --top;
            }
        } else if (c == '+' || c == '*') {
            while (top >= 0 && stack[top] != '(' &&
                   prec_sf(stack[top]) >= prec_sf(c)) {
                out[oi++] = stack[top--];
            }
            stack[++top] = c;
        }
    }

    while (top >= 0) {
        out[oi++] = stack[top--];
    }
    out[oi] = '\0';

    free(stack);
    return out;
}

static int is_temp_matrix(const matrix_sf *m) {
    return !(m->name >= 'A' && m->name <= 'Z');
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *post = infix2postfix_sf(expr);

    size_t n = strlen(post);
    matrix_sf **stack = malloc((n + 8) * sizeof(matrix_sf*));
    int top = -1;

    for (size_t i = 0; i < n; ++i) {
        char c = post[i];

        if (c >= 'A' && c <= 'Z') {
            matrix_sf *m = find_bst_sf(c, root);
            stack[++top] = m;
        } else if (c == '\'') {
            matrix_sf *a = stack[top--];
            matrix_sf *t = transpose_mat_sf(a);
            stack[++top] = t;
            if (is_temp_matrix(a)) {
                free((matrix_sf*)a);
            }
        } else if (c == '+' || c == '*') {
            matrix_sf *b = stack[top--];
            matrix_sf *a = stack[top--];
            matrix_sf *r = (c == '+') ? add_mats_sf(a, b) : mult_mats_sf(a, b);
            stack[++top] = r;
            if (is_temp_matrix(a)) {
                free(a);
            }
            if (is_temp_matrix(b)) {
                free(b);
            }
        }
    }

    matrix_sf *res = stack[top];
    free(stack);
    free(post);

    res->name = name;
    return res;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        return NULL;
    }

    bst_sf *root = NULL;
    matrix_sf *last = NULL;
    char line[8192];

    while (fgets(line, sizeof(line), fp)) {
        char *p = line;

        while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') {
            ++p;
        }
        if (*p == '\0') {
            continue;
        }

        char name = *p++;
        while (*p && *p != '=') {
            ++p;
        }
        if (*p == '=') {
            ++p;
        }

        while (*p == ' ' || *p == '\t') {
            ++p;
        }

        int is_literal = 0;
        for (char *q = p; *q != '\0'; ++q) {
            if (*q == '[') {
                is_literal = 1;
                break;
            }
            if (*q == '\n' || *q == '\r') {
                break;
            }
        }

        matrix_sf *m;
        if (is_literal) {
            m = create_matrix_sf(name, p);
        } else {
            m = evaluate_expr_sf(name, p, root);
        }

        root = insert_bst_sf(m, root);
        last = m;
    }

    fclose(fp);
    return last;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
