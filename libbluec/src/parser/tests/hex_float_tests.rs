// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::super::recursive_descent::hex_float::*;

#[cfg(unix)]
#[test]
fn errno_test() {
    let err_loc = errno_location();
    assert!(!err_loc.is_null());

    clear_errno();
    assert_eq!(get_errno(), 0);
}

#[test]
fn parse_as_f64_test() {
    assert_eq!(parse_as_f64("0xFFp0"), Ok(255.0));
    assert_eq!(parse_as_f64("0x1.8p-1"), Ok(0.75));
    assert_eq!(parse_as_f64("0xFFp+2"), Ok(1020.0));
    assert_eq!(parse_as_f64("0x1p0"), Ok(1.0));
    assert_eq!(parse_as_f64("0x1p+0"), Ok(1.0));
    assert_eq!(parse_as_f64("0x1p-0"), Ok(1.0));
    assert_eq!(parse_as_f64("0x0.1p4"), Ok(1.0));
    assert_eq!(parse_as_f64("0x0.0001p+16"), Ok(1.0));
    assert_eq!(parse_as_f64("0x1.0p10"), Ok(1024.0));
    assert_eq!(parse_as_f64("0x1.921fb54442d18p+0"), Ok(1.5707963267948966));
    assert_eq!(parse_as_f64("0x1.921FB54442D18P+0"), Ok(1.5707963267948966));
    assert_eq!(parse_as_f64("0x1.fffffffffffffp+1023"), Ok(1.7976931348623157e308));
    assert_eq!(parse_as_f64("0x1.0p-1022"), Ok(2.2250738585072014e-308));
    assert_eq!(parse_as_f64("0x1.0p-1074"), Ok(5e-324));
    assert_eq!(parse_as_f64("0x1.2p3"), Ok(9.0));
    assert_eq!(parse_as_f64("0xA.BCp-4"), Ok(0.6708984375));
    assert_eq!(parse_as_f64("0xAbCd.1234p+5"), Ok(1407394.275390625));
    assert_eq!(parse_as_f64("0x0p0"), Ok(0.0));
    assert_eq!(parse_as_f64("0x0p+0"), Ok(0.0));
    assert_eq!(parse_as_f64("0x0p-0"), Ok(0.0));
    assert_eq!(parse_as_f64("0x10p+1"), Ok(32.0));
    assert_eq!(parse_as_f64("0x10.0p+1"), Ok(32.0));
    assert_eq!(parse_as_f64("0xDEAD.BEEFp-8"), Ok(222.67869466543198));
    assert_eq!(parse_as_f64("0xdead.beefP8"), Ok(14593470.93359375));
    assert_eq!(parse_as_f64("0xF.Fp+7"), Ok(2040.0));
    assert_eq!(parse_as_f64("0x1.0000000000001p-1"), Ok(0.5000000000000001));
    assert_eq!(parse_as_f64("0x1.0000000000000p-1"), Ok(0.5));
    assert_eq!(parse_as_f64("0x3.fp+2"), Ok(15.75));
    assert_eq!(parse_as_f64("0x3.P+2"), Ok(12.0));
    assert_eq!(parse_as_f64("0x.8p+1"), Ok(1.0));
    assert_eq!(parse_as_f64("0x.8P+1"), Ok(1.0));
    assert_eq!(parse_as_f64("0xABCp-123"), Ok(2.584206780847717e-34));
    assert_eq!(parse_as_f64("0xabc.p123"), Ok(2.922174825933559e40));
    assert_eq!(parse_as_f64("0x1p+2147483647"), Err(ParseHexFloatErr::OutOfRange));
    assert_eq!(parse_as_f64("0x1p-2147483648"), Err(ParseHexFloatErr::OutOfRange));
    assert_eq!(parse_as_f64("0x1.1p1"), Ok(2.125));
    assert_eq!(parse_as_f64("0x1.2p+03"), Ok(9.0));
    assert_eq!(parse_as_f64("0x1.2p-003"), Ok(0.140625));
    assert_eq!(parse_as_f64("0x1.2P3"), Ok(9.0));
    assert_eq!(parse_as_f64("0x1.2p+3"), Ok(9.0));
    assert_eq!(parse_as_f64("0x1p+1"), Ok(2.0));
    assert_eq!(parse_as_f64("0x1.p+1"), Ok(2.0));
    assert_eq!(parse_as_f64("0x1.0p-1"), Ok(0.5));
    assert_eq!(parse_as_f64("0xF.FFFFFFp-127"), Ok(9.403954771545838e-38));
    assert_eq!(parse_as_f64("0x1.fffffffep+127"), Ok(3.402823668417103e38));

    assert_eq!(
        parse_as_f64(
            "0x10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000p-1474"
        ),
        Ok(4.94066e-324)
    );
    assert_eq!(
        parse_as_f64(
            "0x10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000p1474"
        ),
        Err(ParseHexFloatErr::OutOfRange)
    );
}

#[test]
fn parse_as_f32_test() {
    assert_eq!(parse_as_f32("0xFFp0"), Ok(255.0));
    assert_eq!(parse_as_f32("0xFFp0f"), Ok(255.0));
    assert_eq!(parse_as_f32("0x1.8p-1f"), Ok(0.75));
    assert_eq!(parse_as_f32("0xFFp+2f"), Ok(1020.0));
    assert_eq!(parse_as_f32("0x1p0f"), Ok(1.0));
    assert_eq!(parse_as_f32("0x1p+0f"), Ok(1.0));
    assert_eq!(parse_as_f32("0x1p-0f"), Ok(1.0));
    assert_eq!(parse_as_f32("0x0.1p4f"), Ok(1.0));
    assert_eq!(parse_as_f32("0x0.0001p+16f"), Ok(1.0));
    assert_eq!(parse_as_f32("0x1.0p10f"), Ok(1024.0));
    assert_eq!(parse_as_f32("0x1.921fb54442d18p+0f"), Ok(1.5707963267948966));
    assert_eq!(parse_as_f32("0x1.921FB54442D18P+0f"), Ok(1.5707963267948966));
    assert_eq!(parse_as_f32("0x1.fffffffffffffp+1023f"), Err(ParseHexFloatErr::OutOfRange));
    assert_eq!(parse_as_f32("0x1.0p-1022f"), Err(ParseHexFloatErr::OutOfRange));
    assert_eq!(parse_as_f32("0x1.0p-1074f"), Err(ParseHexFloatErr::OutOfRange));
    assert_eq!(parse_as_f32("0x1.2p3f"), Ok(9.0));
    assert_eq!(parse_as_f32("0xA.BCp-4f"), Ok(0.6708984375));
    assert_eq!(parse_as_f32("0xAbCd.1234p+5f"), Ok(1407394.275390625));
    assert_eq!(parse_as_f32("0x0p0f"), Ok(0.0));
    assert_eq!(parse_as_f32("0x0p+0f"), Ok(0.0));
    assert_eq!(parse_as_f32("0x0p-0f"), Ok(0.0));
    assert_eq!(parse_as_f32("0x10p+1f"), Ok(32.0));
    assert_eq!(parse_as_f32("0x10.0p+1f"), Ok(32.0));
    assert_eq!(parse_as_f32("0xDEAD.BEEFp-8"), Ok(222.67869466543198));
    assert_eq!(parse_as_f32("0xdead.beefP8"), Ok(14593470.93359375));
    assert_eq!(parse_as_f32("0xF.Fp+7"), Ok(2040.0));
    assert_eq!(parse_as_f32("0x1.0000000000001p-1"), Ok(0.5000000000000001));
    assert_eq!(parse_as_f32("0x1.0000000000000p-1"), Ok(0.5));
    assert_eq!(parse_as_f32("0x3.fp+2"), Ok(15.75));
    assert_eq!(parse_as_f32("0x3.P+2"), Ok(12.0));
    assert_eq!(parse_as_f32("0x.8p+1"), Ok(1.0));
    assert_eq!(parse_as_f32("0x.8P+1"), Ok(1.0));
    assert_eq!(parse_as_f32("0x.8p+1f"), Ok(1.0));
    assert_eq!(parse_as_f32("0x.8P+1f"), Ok(1.0));
    assert_eq!(parse_as_f32("0xABCp-123"), Ok(2.584206780847717e-34));
    assert_eq!(parse_as_f32("0xabc.p123"), Err(ParseHexFloatErr::OutOfRange));
    assert_eq!(parse_as_f32("0x1p+2147483647"), Err(ParseHexFloatErr::OutOfRange));
    assert_eq!(parse_as_f32("0x1p-2147483648"), Err(ParseHexFloatErr::OutOfRange));
    assert_eq!(parse_as_f32("0x1.1p1"), Ok(2.125));
    assert_eq!(parse_as_f32("0x1.2p+03"), Ok(9.0));
    assert_eq!(parse_as_f32("0x1.2p-003"), Ok(0.140625));
    assert_eq!(parse_as_f32("0x1.2P3"), Ok(9.0));
    assert_eq!(parse_as_f32("0x1.2p+3"), Ok(9.0));
    assert_eq!(parse_as_f32("0x1p+1"), Ok(2.0));
    assert_eq!(parse_as_f32("0x1.p+1"), Ok(2.0));
    assert_eq!(parse_as_f32("0x1.0p-1"), Ok(0.5));
    assert_eq!(parse_as_f32("0xF.FFFFFFp-127"), Ok(9.403954771545838e-38));
    assert_eq!(parse_as_f32("0x1.fffffffep+63"), Ok(1.8446744e19));
    assert_eq!(parse_as_f32("0x1.fffffffep+127"), Err(ParseHexFloatErr::OutOfRange));

    assert_eq!(
        parse_as_f32(
            "0x10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000p-1474"
        ),
        Err(ParseHexFloatErr::OutOfRange)
    );
    assert_eq!(
        parse_as_f32(
            "0x10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000p1474"
        ),
        Err(ParseHexFloatErr::OutOfRange)
    );
}