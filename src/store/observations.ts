import { defineStore } from 'pinia'
import { ref } from 'vue'
import { Datastream } from '@/types'
import { fetchObservationsSync } from '@/utils/observationsUtils'
import { ObservationRecord } from '@/utils/plotting/observationRecordV2'

export const useObservationStore = defineStore(
  'observations',
  () => {
    const observations = ref<Record<string, ObservationRecord>>({}) // TODO: make persistent
    const observationsRaw = ref<
      Record<string, { datetimes: number[]; values: number[] }>
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

      const existingRecord = observations.value[id]
      let beginDataPromise: Promise<{ datetimes: number[]; values: number[] }> =
        Promise.resolve({ datetimes: [], values: [] })
      let endDataPromise: Promise<{ datetimes: number[]; values: number[] }> =
        Promise.resolve({ datetimes: [], values: [] })

      if (observationsRaw.value[id]?.values.length) {
        const rawBeginDatetime: Date = new Date(
          observationsRaw.value[id].datetimes[0]
        )
        const rawEndDatetime: Date = new Date(
          observationsRaw.value[id].datetimes[
            observationsRaw.value[id].datetimes.length - 1
          ]
        )

        // Check if new data before the stored data is needed
        if (beginTime < rawBeginDatetime) {
          beginDataPromise = fetchObservationsSync(
            datastream,
            beginTime,
            rawBeginDatetime
          )
        }

        // Check if new data after the stored data is needed
        if (endTime > rawEndDatetime) {
          const temp = rawEndDatetime
          temp.setSeconds(temp.getSeconds() + 1)

          endDataPromise = fetchObservationsSync(datastream, temp, endTime)
        }
      } else {
        // There is no raw data for this dataset. Fetch the whole range
        beginDataPromise = fetchObservationsSync(datastream, beginTime, endTime)
      }

      // Fetch and update in parallel if needed
      const [beginData, endData] = await Promise.all([
        beginDataPromise,
        endDataPromise,
      ])

      if (!observationsRaw.value[id]) {
        observationsRaw.value[id] = { datetimes: [], values: [] }
        // observationsRaw.value[id].datetimes = []
      }

      if (beginData.values.length > 0) {
        observationsRaw.value[id] = {
          datetimes: [
            ...beginData.datetimes,
            ...observationsRaw.value[id].datetimes,
          ],
          values: [...beginData.values, ...observationsRaw.value[id].values],
        }
      }

      if (endData.values.length > 0) {
        observationsRaw.value[id] = {
          datetimes: [
            ...observationsRaw.value[id].datetimes,
            ...endData.datetimes,
          ],
          values: [...observationsRaw.value[id].values, ...endData.values],
        }
      }

      return existingRecord
    }

    return {
      observations,
      observationsRaw,
      fetchObservationsInRange,
    }
  },
  {
    persist: {
      pick: ['observationsRaw'],
    },
  }
)
