self.addEventListener(
  'message',
  function (e: MessageEvent) {
    const dataX = new Float64Array(e.data.bufferX)
    const indices = new Uint32Array(e.data.indices)

    for (let i = e.data.range[0]; i <= e.data.range[1]; i++) {
      dataX[indices[i]] = -Infinity
    }

    self.postMessage('Done')
  },
  false
)

// postMessage('hi from worker') // Send this back to the main script.
