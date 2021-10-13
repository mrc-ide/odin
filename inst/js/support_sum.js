function odinSum2(x, iFrom, iTo, jFrom, jTo, dim1) {
  var tot = 0.0;
  for (var j = jFrom; j < jTo; ++j) {
    var jj = j * dim1;
    for (var i = iFrom; i < iTo; ++i) {
      tot += x[i + jj];
    }
  }
  return tot;
}
function odinSum3(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, dim1, dim12) {
  var tot = 0.0;
  for (var k = kFrom; k < kTo; ++k) {
    var kk = k * dim12;
    for (var j = jFrom; j < jTo; ++j) {
      var jj = j * dim1 + kk;
      for (var i = iFrom; i < iTo; ++i) {
        tot += x[i + jj];
      }
    }
  }
  return tot;
}
function odinSum4(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, dim1, dim12, dim123) {
  var tot = 0.0;
  for (var l = lFrom; l < lTo; ++l) {
    var ll = l * dim123;
    for (var k = kFrom; k < kTo; ++k) {
      var kk = k * dim12 + ll;
      for (var j = jFrom; j < jTo; ++j) {
        var jj = j * dim1 + kk;
        for (var i = iFrom; i < iTo; ++i) {
          tot += x[i + jj];
        }
      }
    }
  }
  return tot;
}
function odinSum5(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, i5From, i5To, dim1, dim12, dim123, dim1234) {
  var tot = 0.0;
  for (var i5 = i5From; i5 < i5To; ++i5) {
    var i5i5 = i5 * dim1234;
    for (var l = lFrom; l < lTo; ++l) {
      var ll = l * dim123 + i5i5;
      for (var k = kFrom; k < kTo; ++k) {
        var kk = k * dim12 + ll;
        for (var j = jFrom; j < jTo; ++j) {
          var jj = j * dim1 + kk;
          for (var i = iFrom; i < iTo; ++i) {
            tot += x[i + jj];
          }
        }
      }
    }
  }
  return tot;
}
function odinSum6(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, i5From, i5To, i6From, i6To, dim1, dim12, dim123, dim1234, dim12345) {
  var tot = 0.0;
  for (var i6 = i6From; i6 < i6To; ++i6) {
    var i6i6 = i6 * dim12345;
    for (var i5 = i5From; i5 < i5To; ++i5) {
      var i5i5 = i5 * dim1234 + i6i6;
      for (var l = lFrom; l < lTo; ++l) {
        var ll = l * dim123 + i5i5;
        for (var k = kFrom; k < kTo; ++k) {
          var kk = k * dim12 + ll;
          for (var j = jFrom; j < jTo; ++j) {
            var jj = j * dim1 + kk;
            for (var i = iFrom; i < iTo; ++i) {
              tot += x[i + jj];
            }
          }
        }
      }
    }
  }
  return tot;
}
function odinSum7(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, i5From, i5To, i6From, i6To, i7From, i7To, dim1, dim12, dim123, dim1234, dim12345, dim123456) {
  var tot = 0.0;
  for (var i7 = i7From; i7 < i7To; ++i7) {
    var i7i7 = i7 * dim123456;
    for (var i6 = i6From; i6 < i6To; ++i6) {
      var i6i6 = i6 * dim12345 + i7i7;
      for (var i5 = i5From; i5 < i5To; ++i5) {
        var i5i5 = i5 * dim1234 + i6i6;
        for (var l = lFrom; l < lTo; ++l) {
          var ll = l * dim123 + i5i5;
          for (var k = kFrom; k < kTo; ++k) {
            var kk = k * dim12 + ll;
            for (var j = jFrom; j < jTo; ++j) {
              var jj = j * dim1 + kk;
              for (var i = iFrom; i < iTo; ++i) {
                tot += x[i + jj];
              }
            }
          }
        }
      }
    }
  }
  return tot;
}
function odinSum8(x, iFrom, iTo, jFrom, jTo, kFrom, kTo, lFrom, lTo, i5From, i5To, i6From, i6To, i7From, i7To, i8From, i8To, dim1, dim12, dim123, dim1234, dim12345, dim123456, dim1234567) {
  var tot = 0.0;
  for (var i8 = i8From; i8 < i8To; ++i8) {
    var i8i8 = i8 * dim1234567;
    for (var i7 = i7From; i7 < i7To; ++i7) {
      var i7i7 = i7 * dim123456 + i8i8;
      for (var i6 = i6From; i6 < i6To; ++i6) {
        var i6i6 = i6 * dim12345 + i7i7;
        for (var i5 = i5From; i5 < i5To; ++i5) {
          var i5i5 = i5 * dim1234 + i6i6;
          for (var l = lFrom; l < lTo; ++l) {
            var ll = l * dim123 + i5i5;
            for (var k = kFrom; k < kTo; ++k) {
              var kk = k * dim12 + ll;
              for (var j = jFrom; j < jTo; ++j) {
                var jj = j * dim1 + kk;
                for (var i = iFrom; i < iTo; ++i) {
                  tot += x[i + jj];
                }
              }
            }
          }
        }
      }
    }
  }
  return tot;
}
