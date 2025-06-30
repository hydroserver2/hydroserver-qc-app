import { beforeEach, describe, expect, it } from 'vitest'
import { mockDatastream } from './mock'
import { ObservationRecord } from '../observationRecord'

const datetimes = new Float64Array(
  new ArrayBuffer(
    mockDatastream.phenomenon_time.length * Float64Array.BYTES_PER_ELEMENT,
    {
      maxByteLength: mockDatastream.phenomenon_time.length * Float64Array.BYTES_PER_ELEMENT, // Max size the array can reach
    }
  )
)
const dataValues = new Float32Array(
  new ArrayBuffer(
    mockDatastream.result.length * Float32Array.BYTES_PER_ELEMENT,
    {
      maxByteLength: mockDatastream.result.length * Float32Array.BYTES_PER_ELEMENT, // Max size the array can reach
    }
  )
)

datetimes.set(mockDatastream.phenomenon_time.map(dateString => new Date(dateString).getTime()))
dataValues.set(mockDatastream.result)


const rawData: {
  datetimes: Float64Array<ArrayBuffer>
  dataValues: Float32Array<ArrayBuffer>
} = { datetimes, dataValues }

const obsRecord = new ObservationRecord(rawData)

beforeEach(async () => {
  await obsRecord.reload()
  return
})


describe('observationRecord', () => {
  it('loads data', async () => {
    const length = obsRecord.dataX.length
    expect(length).toBe(rawData.datetimes.length)
    // const measurement = await measureEllapsedTime(doSomething)
    // expect(measurement.duration).to.be.greaterThanOrEqual(duration)
  })
})