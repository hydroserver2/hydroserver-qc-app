import { defineStore } from 'pinia'
import { ref } from 'vue'
import { Datastream } from '@/types'
import { fetchObservationsSync } from '@/utils/observationsUtils'
import { ObservationRecord } from '@/utils/plotting/observationRecord'

export const useObservationStore = defineStore(
  'observations',
  () => {
    const observations = ref<Record<string, ObservationRecord>>({})
    const observationsRaw = ref<
      Record<
        string,
        {
          datetimes: Float64Array<ArrayBuffer>
          dataValues: Float32Array<ArrayBuffer>
        }
      >
    >({})

    /**
     * Fetches requested observations that aren't currently in the pinia store,
     * updates the store, then returns the corresponding `ObservationRecord`.
     */
    const fetchObservationsInRange = async (
      datastream: Datastream,
      beginTime: Date,
      endTime: Date
    ): Promise<ObservationRecord> => {
      console.log('fetchObservationsInRange')
      const id = datastream.id

      // If nothing is stored yet, create a new record
      if (!observations.value[id]) {
        observations.value[id] = new ObservationRecord(datastream)
      }

      let beginDataPromise: Promise<{
        datetimes: number[]
        dataValues: number[]
      }> = Promise.resolve({ datetimes: [], dataValues: [] })
      let endDataPromise: Promise<{
        datetimes: number[]
        dataValues: number[]
      }> = Promise.resolve({ datetimes: [], dataValues: [] })

      if (observationsRaw.value[id]?.dataValues.length) {
        const rawBeginDatetime = new Date(
          observationsRaw.value[id].datetimes[0]
        )

        // Check if new data before the stored data is needed
        if (beginTime <= rawBeginDatetime) {
          // rawBeginDatetime.setSeconds(rawBeginDatetime.getSeconds() - 1) // Results in range will be inclusive, so we need to offset by 1
          beginDataPromise = fetchObservationsSync(
            datastream,
            beginTime,
            rawBeginDatetime
          )
        }

        const rawEndDatetime = new Date(
          observationsRaw.value[id].datetimes[
            observationsRaw.value[id].datetimes.length - 1
          ]
        )

        // Check if new data after the stored data is needed
        if (endTime >= rawEndDatetime) {
          rawEndDatetime.setSeconds(rawEndDatetime.getSeconds() + 1)
          endDataPromise = fetchObservationsSync(
            datastream,
            rawEndDatetime,
            endTime
          )
        }
      } else {
        const endTimeCopy = new Date(endTime)
        endTimeCopy.setSeconds(endTimeCopy.getSeconds() + 1)
        // Right value is exclusive
        beginDataPromise = fetchObservationsSync(
          datastream,
          beginTime,
          endTimeCopy
        )
      }

      const [beginData, endData] = await Promise.all([
        beginDataPromise,
        endDataPromise,
      ])

      if (!observationsRaw.value[id]) {
        observationsRaw.value[id] = {
          datetimes: new Float64Array(0),
          dataValues: new Float32Array(0),
        }
      }

      if (beginData.dataValues.length > 0 || endData.dataValues.length > 0) {
        const newLength =
          beginData.dataValues.length +
          endData.dataValues.length +
          observationsRaw.value[id].dataValues.length
        const newBufferX = new ArrayBuffer(
          newLength * Float64Array.BYTES_PER_ELEMENT
        )
        const newBufferY = new ArrayBuffer(
          newLength * Float32Array.BYTES_PER_ELEMENT
        )

        const newArrayX = new Float64Array(newBufferX)
        const newArrayY = new Float32Array(newBufferY)

        // Begin data
        let offset = 0
        newArrayX.set(beginData.datetimes, offset)
        newArrayY.set(beginData.dataValues, offset)

        // Previous data
        offset += beginData.datetimes.length
        newArrayX.set(observationsRaw.value[id].datetimes, offset)
        newArrayY.set(observationsRaw.value[id].dataValues, offset)

        // End data
        offset += endData.datetimes.length
        newArrayX.set(endData.datetimes, offset)
        newArrayY.set(endData.dataValues, offset)

        observationsRaw.value[id].datetimes = newArrayX
        observationsRaw.value[id].dataValues = newArrayY
      }

      if (beginData.dataValues.length || endData.dataValues.length) {
        observations.value[id].loadData(observationsRaw.value[id])
      }

      return observations.value[id]
    }

    return {
      observations,
      observationsRaw,
      fetchObservationsInRange,
    }
  },
  {
    persist: {
      pick: [
        // TODO: enable only in development mode
        // 'observationsRaw', // TODO: can not save buffers correctly
      ],
    },
  }
)
