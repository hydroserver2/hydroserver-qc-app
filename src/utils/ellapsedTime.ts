export const measureEllapsedTime = async (
  fn: () => any,
  message?: string
): Promise<{ response: any; duration: number }> => {
  if (message) {
    console.log(message)
  }
  const start = performance.now()
  const response = await fn()
  const end = performance.now()
  console.log(`\tDone in ${(end - start).toFixed(2)} ms`)
  const duration = +(end - start)
  return { response, duration }
}
