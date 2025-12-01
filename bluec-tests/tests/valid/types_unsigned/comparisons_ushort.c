int main(void) {
  unsigned short us;
  unsigned int ui;
  int si;
  short ss;
  
  // Test 1: == with unsigned int (equal)
  us = 100;
  ui = 100;
  if (!(us == ui)) return 1;
  
  // Test 2: != with unsigned int (not equal)
  us = 100;
  ui = 200;
  if (!(us != ui)) return 2;
  
  // Test 3: < with unsigned int (less than)
  us = 100;
  ui = 200;
  if (!(us < ui)) return 3;
  
  // Test 4: > with unsigned int (greater than)
  us = 200;
  ui = 100;
  if (!(us > ui)) return 4;
  
  // Test 5: <= with unsigned int (less or equal, equal case)
  us = 100;
  ui = 100;
  if (!(us <= ui)) return 5;
  
  // Test 6: >= with unsigned int (greater or equal, equal case)
  us = 100;
  ui = 100;
  if (!(us >= ui)) return 6;
  
  // Test 7: == with signed int (positive, equal)
  us = 100;
  si = 100;
  if (!(us == si)) return 7;
  
  // Test 8: < with signed int (positive)
  us = 100;
  si = 200;
  if (!(us < si)) return 8;
  
  // Test 9: > with negative signed int (unsigned always greater)
  us = 10;
  si = -100;
  if (!(us > si)) return 9;
  
  // Test 10: != with negative signed int
  us = 0;
  si = -1;
  if (!(us != si)) return 10;
  
  // Test 11: == with signed short (equal)
  us = 1000;
  ss = 1000;
  if (!(us == ss)) return 11;
  
  // Test 12: != with signed short (negative)
  us = 100;
  ss = -100;
  if (!(us != ss)) return 12;
  
  // Test 13: <= with unsigned int (less case)
  us = 50;
  ui = 100;
  if (!(us <= ui)) return 13;
  
  // Test 14: >= with unsigned int (greater case)
  us = 200;
  ui = 100;
  if (!(us >= ui)) return 14;
  
  // Test 15: Comparing USHRT_MAX with larger unsigned int
  us = 65535;
  ui = 65535 + 1;
  if (!(us < ui)) return 15;
  
  // Test 16: Comparing 0 with signed int 0
  us = 0;
  si = 0;
  if (!(us == si)) return 16;
  
  // Test 17: Comparison chain (us between two values)
  us = 100;
  if (!(50 < us && us < 150)) return 17;
  
  // Test 18: Max unsigned short vs max signed short
  us = 65535;
  ss = 32767;
  if (!(us > ss)) return 18;
  
  // Test 19: < with signed short (negative vs positive)
  us = 100;
  ss = -1;
  if (!(ss < us)) return 19;
  
  // Test 20: >= with signed short at boundary
  us = 32767;
  ss = 32767;
  if (!(us >= ss)) return 20;
  
  return 0; // success
}