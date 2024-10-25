export const formatDate = (date: Date) => {
  return date.toLocaleString(undefined, {
    year: 'numeric',
    month: 'short',
    day: '2-digit',
    hour: '2-digit',
    hour12: false,
    minute: '2-digit',
    second: '2-digit',
  })
}
