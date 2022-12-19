bool c;

function main() gives int {
  console << main1(true, false);
  console << main1(false, false);

  return 0;
}

function main1(bool a, bool b) gives bool {
  if (a == b) {
    c = true;
  } else {
    c = false;
  }
  return c;
}