function main(int x) gives int{

    /* This is simple data type declaration */
    int_var = int 3;
    f_var = float 4.0;
    bool_var = bool true;
    bool_var2 = bool False;

    /* print the single data type's values*/
    console<<int_var;
    consolef<<f_var;
    console<<bool_var;
    console<<bool_var2;


    /* This is the matrix declaration */
    i_matrix1d = mat int [3];
    f_matrix2d = mat mat float [3] [4];

    /* print the matrix data type's values*/
    i_matrix1d[1] = 1;
    i_matrix1d[2] = 2;
    console<<(i_matrix1d[1] + i_matrix1d[2]);
    console<<i_matrix1d[1];
    console<<i_matrix1d[2];

    f_matrix2d[0][1] = 1.0;
    f_matrix2d[1][2] = 2.0;
    f_matrix2d[2][3] = 3.0;

    consolef<<f_matrix2d[0][1];
    consolef<<f_matrix2d[1][2];
    consolef<<f_matrix2d[2][3];

    
    return 0;
}