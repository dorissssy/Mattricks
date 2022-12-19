bool c;

function main() gives int {
  console << main1();
  console << main2();
  console << main3();
  console << main4();
  console << main5();
  console << main6();
  console << main7();
  console << main8();
  console << main9();
  console << main10();
  console << main11();
  console << main12();
  console << main13();
  console << main14();
  /*console << main15();*/

  return 0;
}

/* Equal */
function main1() gives bool {
  a = int 1;
  b = int 1;
  c = a == b;
  return c;
}

function main2() gives bool {
  a = bool true;
  b = bool true;
  c = a == b;
  return c;
}

function main3() gives bool {
  a = float 1.23;
  b = float 1.23;
  c = a == b;
  return c;
}

/* Not Equal */
function main4() gives bool {
  a = int 1;
  b = int 2;
  c = a != b;
  return c;
}

function main5() gives bool {
  a = bool true;
  b = bool true;
  c = a != b;
  return c;
}

function main6() gives bool {
  a = float .1;
  b = float 2.;
  c = a != b;
  return c;
}

/* Greater than */
function main7() gives bool {
  a = int 1;
  b = int 2;
  c = a > b;
  return c;
}

function main8() gives bool {
  a = float .1;
  b = float 2.;
  c = a > b;
  return c;
}

/* Less than */
function main9() gives bool {
  a = int 1;
  b = int 2;
  c = a < b;
  return c;
}

function main10() gives bool {
  a = float .1;
  b = float 2.;
  c = a < b;
  return c;
}

/* Greater than or equal to */
function main11() gives bool {
  a = int 1;
  b = int 2;
  c = a >= b;
  return c;
}

function main12() gives bool {
  a = float .1;
  b = float 2.;
  c = a >= b;
  return c;
}

/* Less than or equal to */
function main13() gives bool {
  a = int 1;
  b = int 2;
  c = a <= b;
  return c;
}

function main14() gives bool {
  a = float .1;
  b = float 2.;
  c = a <= b;
  return c;
}

/* Negation */
/*
function main15() gives bool {
  b = bool true;
  return !b;
}
*/
