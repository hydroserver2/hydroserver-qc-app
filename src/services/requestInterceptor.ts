/**
 * Intercepts and enhances a request options object.
 *
 * - If an access token is provided, the function adds an 'Authorization' header.
 * - If a body is present and it's an object, the body is stringified.
 *
 * @param {any} options - The original request options object.
 * @param {string} accessToken - The access token to be added to the request headers.
 *
 * @returns {any} The enhanced request options with possible modified headers and body.
 */
export function requestInterceptor(options: any, accessToken: string) {
  let headers = options.headers ? { ...options.headers } : {}

  if (accessToken) headers['Authorization'] = `Bearer ${accessToken}`

  let body: string | undefined = undefined
  if (options.body !== undefined) {
    body =
      typeof options.body === 'string' || options.body instanceof FormData
        ? options.body
        : JSON.stringify(options.body)
  }
  return {
    ...options,
    headers: headers,
    body: body,
  }
}
