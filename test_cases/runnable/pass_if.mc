function main() gives int {
  console << main1(true, true);
  console << main1(true, false);
  return 0;
}

function main1(bool a, bool b) gives bool {
  if (a == b) {
    return true;
  }
  return false;
}