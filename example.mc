function m(mat int [4] m) gives mat int [4] {
    m[0] = m[0] + 1;
    console<<m[0];
    return m;
}

int i;

function init_val_1d_int_10(mat int [10] m, int val) gives mat int [10] {
    i = 0;
    while (i < 10) {
        m[i] = val;
        i = i + 1;
    }
    return m;
}

function print_1d_int_10(mat int [10] m) gives int {
    i = 0;
    while (i < 10) {
        console << m[i];
        i = i + 1;
    }
    return 0;
}

function addval_1d_int_10(mat int [10] m, int val) gives mat int [10] {
    i = 0;
    while (i < 10) {
        m[i] = m[i] + val;
        i = i + 1;
    }
    return m;
}

int v1;
int v2;
int j;

function add_int_int(int x, int y) gives int {
    j = x + y;
    return j;
}

function addmat_1d_int_10(mat int [10] m1, mat int [10] m2) gives mat int [10] {
    i = 0;
    ii = int 0;
    while (ii < 10) {
        v1 = m1[ii];
        v2 = m2[ii];
        m1[ii] = add_int_int(v1, v2);
        console << 12345;
        ii = ii + 1;
    }
    return m1;
}

function init_val_2d_int_6_7(mat mat int [6][7] m, int val) gives mat mat int [6][7] {
    i = 0;
    j = 0;
    while (i < 5) {
        console<<i;
        console<<j;
        m[i][j] = val;
        j = j + 1;
        if (j > 6) {
            j = 0;
            i = i + 1;
        }
    }
    return m;
}


function main(int x) gives int{
    
    mat_1d_int_10 = mat int [10];
    mat2_1d_int_10 = mat int [10];

    mat_1d_int_10 = init_val_1d_int_10(mat_1d_int_10, 0); /* init */
    print_1d_int_10(mat_1d_int_10);

    mat_1d_int_10 = init_val_1d_int_10(mat_1d_int_10, 123); /* init */
    mat_1d_int_10 = addval_1d_int_10(mat_1d_int_10, -123); /* add all with val */








/*

    mat2_1d_int_10 = init_val_1d_int_10(mat2_1d_int_10, 12);/* init */

    mat_1d_int_10 = addmat_1d_int_10(mat_1d_int_10, mat2_1d_int_10);
    print_1d_int_10(mat_1d_int_10);

    
    mat_2d_int_6_7 = mat mat int [6][7];
    mat_2d_int_6_7[1][2] = 4;
    console << mat_2d_int_6_7[1][2] ;


    mat_2d_int_6_7 = init_val_2d_int_6_7(mat_2d_int_6_7, 456);

        mat_1d_int_10[1] = 1;
    print_1d_int_10(mat_1d_int_10);


    a = int 3;
    f = float 3.33333;
    f = f+0.1;
    consolef<<f;

    ff = mat mat float [3] [4];
    ff2  = mat mat float [3] [4];

    ff[1][1] = 1.0;
    ff[1][2] = 2.0;
    ff[1][3] = 3.0;
    ff[2][3] = 4.0;
    ff2[2][3] = 5.0;
    consolef<<ff[1][1];
    consolef<<ff[1][2];
    consolef<<ff[1][3];
    consolef<<ff[2][3];
    consolef<<ff2[2][3];
    consolef<<ff[1][1];


    



    f_float = mat float [4];
    f_float[0] = 0.0;
    console<<f_float[0];
    fb = mat int [4];
    fb[0] = 0;
    fb = m(fb);
    console<<fb[0];

     ff[0][0];

    ffb = mat mat bool [5] [4];
    ffb[0][0];

    ffi = mat mat int [5] [4];
    ffi[0][0];


*/


/*   
    fff = mat float [4];
    fff[0];

    ffff = mat mat mat float [3][4][5];
    ffff[0][0][0];
   
    f = 4.4444444;
    console<<a;
    consolef<<f;
    console<<3;
    consolef<<3.33;
    console<<true;
    console<<false;
*/
    return 0;
}