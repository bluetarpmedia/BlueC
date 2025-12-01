int main(void) {
  unsigned short us;
  unsigned short us2;
  unsigned int ui;
  int si;
  short ss;
  unsigned long ul;
  long sl;
  
  // Test 1: Cast to unsigned int and back (value fits)
  us = 1000;
  ui = (unsigned int)us;
  us2 = (unsigned short)ui;
  if (us2 != 1000) return 1;
  
  // Test 2: Cast to int and back (positive value)
  us = 5000;
  si = (int)us;
  us2 = (unsigned short)si;
  if (us2 != 5000) return 2;
  
  // Test 3: Cast to short and back (value fits in signed short)
  us = 1000;
  ss = (short)us;
  us2 = (unsigned short)ss;
  if (us2 != 1000) return 3;
  
  // Test 4: Cast large value to short and back (truncation)
  us = 40000;
  ss = (short)us;
  us2 = (unsigned short)ss;
  if (us2 != 40000) return 4;
  
  // Test 5: Cast USHRT_MAX to int and back
  us = 65535;
  si = (int)us;
  us2 = (unsigned short)si;
  if (us2 != 65535) return 5;
  
  // Test 6: Cast to unsigned long and back
  us = 12345;
  ul = (unsigned long)us;
  us2 = (unsigned short)ul;
  if (us2 != 12345) return 6;
  
  // Test 7: Cast to long and back
  us = 32000;
  sl = (long)us;
  us2 = (unsigned short)sl;
  if (us2 != 32000) return 7;
  
  // Test 8: Chained cast through multiple types
  us = 500;
  us2 = (unsigned short)(int)(unsigned int)us;
  if (us2 != 500) return 8;
  
  // Test 9: Chained cast with sign changes
  us = 100;
  us2 = (unsigned short)(short)(int)us;
  if (us2 != 100) return 9;
  
  // Test 10: Cast negative int to unsigned short
  si = -1;
  us = (unsigned short)si;
  if (us != 65535) return 10;
  
  // Test 11: Cast negative int to unsigned short and back to int
  si = -100;
  us = (unsigned short)si;
  si = (int)us;
  if (si != (int)(65535 - 99)) return 11;
  
  // Test 12: Cast zero through multiple types
  us = 0;
  us2 = (unsigned short)(long)(int)(unsigned int)us;
  if (us2 != 0) return 12;
  
  // Test 13: Cast from unsigned int larger than USHRT_MAX
  ui = 100000;
  us = (unsigned short)ui;
  ui = (unsigned int)us;
  if (ui != (100000 & 0xFFFF)) return 13;
  
  // Test 14: Cast negative short to unsigned short
  ss = -1;
  us = (unsigned short)ss;
  if (us != 65535) return 14;
  
  // Test 15: Round-trip through short (positive)
  us = 30000;
  ss = (short)us;
  us2 = (unsigned short)ss;
  if (us2 != 30000) return 15;
  
  // Test 16: Chained cast with truncation
  ui = 0x12345;
  us = (unsigned short)(int)ui;
  if (us != 0x2345) return 16;
  
  // Test 17: Cast through unsigned long (large value)
  ul = 0x1FFFF;
  us = (unsigned short)ul;
  if (us != 0xFFFF) return 17;
  
  // Test 18: Cast through signed long (negative)
  sl = -500;
  us = (unsigned short)sl;
  if (us != (unsigned short)(65535 - 499)) return 18;
  
  // Test 19: Triple cast chain
  us = 1234;
  us2 = (unsigned short)(unsigned int)(short)us;
  if (us2 != 1234) return 19;
  
  // Test 20: Cast with bit pattern preservation
  us = 0xABCD;
  ui = (unsigned int)us;
  us2 = (unsigned short)ui;
  if (us2 != 0xABCD) return 20;
  
  return 0; // success
}