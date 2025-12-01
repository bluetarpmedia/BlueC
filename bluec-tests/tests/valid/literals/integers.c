int main(void) {
  /* 1: decimal */
  if (42 != 42) return 1;

  /* 2: hexadecimal */
  if (0x2A != 42) return 2;

  /* 3: octal */
  if (052 != 42) return 3;

  if (0b101010 != 42) return 4;

  /* 5: U suffix produces same numeric value as an unsigned cast */
  if (42U != (unsigned)42) return 5;

  /* 6: L suffix produces same numeric value as a long cast */
  if (42L != (long)42) return 6;

  /* 7: LL suffix produces same numeric value as a long long cast */
  if (42LL != (long long)42) return 7;

  /* 8: UL suffix produces same numeric value as an unsigned long cast */
  if (42UL != (unsigned long)42) return 8;

  /* 9: ULL suffix produces same numeric value as an unsigned long long cast */
  if (42ULL != (unsigned long long)42) return 9;

  /* 10: test a large literal that requires wider type and check value */
  /* 0x100000000 is 4294967296 decimal; require ULL suffix to be portable */
  if (0x100000000ULL != (unsigned long long)4294967296ULL) return 10;

  /* 11: unsigned arithmetic wrap behavior (sanity for U suffix) */
  if (! (0U - 1U > 0U) ) return 11;

  /* 12: check that signed subtraction does not wrap the same way */
  if ( (int)(0 - 1) >= 0 ) return 12;

  /* 13: ensure mixing suffixes yields expected type/value for a bigger literal */
  if (0xFFFFFFFFULL != 4294967295ULL) return 13;

  return 0;
}
