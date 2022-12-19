function m(mat int [4] m) gives mat int [4] {
    m[0] = m[0] + 1;
    console<<m[0];
    return m;
}

function main(int x) gives int{
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


    


/*
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