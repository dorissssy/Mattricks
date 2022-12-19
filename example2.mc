function main(int x) gives int{

    /* This is simple data type declaration */
    int_var = int 3;
    f_var = float 4.0;
    bool_var = bool true;


    /* This is the matrix declaration */
    f_matrix2d = mat mat float [3] [4];
    f2_matrix2d = mat mat float [3] [4];
    ff2  = mat mat float [3] [4];
    i_matrix1d = mat int [3];

    /* print the single data type's values*/
    console<<int_var;
    consolef<<f_var;
    console<<bool_var;



    /* print the matrix data type's values*/
    f_matrix2d[1][1] = 1.0;
    f_matrix2d[1][2] = 2.0;
    f_matrix2d[1][3] = 3.0;

    f2_matrix2d[2][3] = 7.0;
    consolef<<f_matrix2d[1][1];
    consolef<<f2_matrix2d[2][3];


    i_matrix1d[1] = 1;
    i_matrix1d[2] = 2;
    console<<(i_matrix1d[1] + i_matrix1d[2]);
    console<<i_matrix1d[1];
    console<<i_matrix1d[2];
    return 0;
}