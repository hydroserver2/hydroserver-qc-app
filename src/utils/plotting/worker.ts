self.addEventListener(
  'message',
  function (e: MessageEvent) {
    // Send the message back.
    const buffer = e.data.data as ArrayBuffer
    const index = e.data.index
    const sharedArray = new Uint32Array(buffer)
    sharedArray[index] = 999
    self.postMessage(sharedArray.buffer)
  },
  false
)

// postMessage('hi from worker') // Send this back to the main script.
