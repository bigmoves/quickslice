/// JavaScript FFI for number formatting

export function formatNumber(number) {
  return new Intl.NumberFormat('en-US').format(number);
}
