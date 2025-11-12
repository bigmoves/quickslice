/// JavaScript FFI for date formatting

export function formatTimeLocal(isoTimestamp) {
  try {
    const date = new Date(isoTimestamp);
    // Format as HH:MM:SS in local timezone
    return date.toLocaleTimeString("en-US", {
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
      hour12: false,
    });
  } catch (_e) {
    return isoTimestamp;
  }
}

export function formatDateTimeLocal(isoTimestamp) {
  try {
    const date = new Date(isoTimestamp);
    // Format as full date and time in local timezone
    return date.toLocaleString("en-US", {
      year: "numeric",
      month: "2-digit",
      day: "2-digit",
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
      hour12: false,
    });
  } catch (_e) {
    return isoTimestamp;
  }
}
