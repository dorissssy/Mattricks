float c;

function main() gives int {
  consolef << main1();
  consolef << main1_2();
  consolef << main2();
  consolef << main2_2();
  consolef << main2_3();
  consolef << main3();
  consolef << main3_2();
  consolef << main3_3();
  consolef << main4();
  consolef << main4_2();
  consolef << main4_3(); 
  return 0;
}

function main1() gives float {
  a = float 123.45;
  b = float 678.90;
  c = a + b;
  return c;
}

function main1_2() gives float {
  a = float -123.;
  b = float -456.;
  c = a + b;
  return c;
}

/* Subtraction */
function main2() gives float {
  a = float 123.;
  b = float 456.;
  return b - a;
}

function main2_2() gives float {
  a = float 123.;
  b = float -456.;
  return b - a;
}

function main2_3() gives float {
  a = float -123.;
  b = float -456.;
  return b - a;
}

function main3() gives float {
  a = float 123.;
  b = float 456.;
  c = a * b;
  return c;
}

function main3_2() gives float {
  a = float -123.;
  b = float 456.;
  c = a * b;
  return c;
}

function main3_3() gives float {
  a = float -123.;
  b = float -456.;
  c = a * b;
  return c;
}

function main4() gives float {
  a = float 123.;
  b = float 456.;
  c = b / a;
  return c;
}

function main4_1() gives float {
  a = float 123.;
  b = float 456.;
  c = a / b;
  return c;
}

function main4_2() gives float {
  a = float 123.;
  b = float -456.;
  c = b / a;
  return c;
}

function main4_3() gives float {
  a = float -123.;
  b = float -456.;
  c = b / a;
  return c;
}

