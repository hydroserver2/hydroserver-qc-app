import { defineStore } from 'pinia'
import { ref } from 'vue'
import { Datastream } from '@/types'
import { fetchObservationsSync } from '@/utils/observationsUtils'
import { ObservationRecord } from '@/utils/plotting/observationRecord'

export const useObservationStore = defineStore(
  'observations',
  () => {
    const observations = ref<Record<string, ObservationRecord>>({}) // TODO: make persistent
    const observationsRaw = ref<Record<string, [string, number, any][]>>({})

    /**
     * Fetches requested observations that aren't currently in the pinia store,
     * updates the store, then returns the requested observations.
     * TODO: this method performs too many iterations over the dataset.
     * TODO: only fetch observations when plotting the series
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
      let beginDataPromise: Promise<any[]> = Promise.resolve([])
      let endDataPromise: Promise<any[]> = Promise.resolve([])

      if (existingRecord.beginTime && existingRecord.endTime) {
        // Check if new data before the stored data is needed
        if (beginTime < existingRecord.beginTime) {
          beginDataPromise = fetchObservationsSync(
            datastream,
            beginTime,
            existingRecord.beginTime
          )
        }

        // Check if new data after the stored data is needed
        if (endTime > existingRecord.endTime) {
          const temp = existingRecord.endTime
          temp.setSeconds(temp.getSeconds() + 1)

          endDataPromise = fetchObservationsSync(datastream, temp, endTime)
        }
      } else {
        // Record has no data at all. Fetch the full range.
        beginDataPromise = fetchObservationsSync(datastream, beginTime, endTime)
      }

      // Fetch and update in parallel if needed
      const [beginData, endData] = await Promise.all([
        beginDataPromise,
        endDataPromise,
      ])

      if (!observationsRaw.value[id]) {
        observationsRaw.value[id] = []
      }

      if (beginData.length > 0) {
        observationsRaw.value[id] = [...beginData, ...observationsRaw.value[id]]
      }

      if (endData.length > 0) {
        observationsRaw.value[id] = [...observationsRaw.value[id], ...endData]
      }

      // If the data has changed, renegerate the dataset
      if (beginData.length > 0 || endData.length > 0) {
        existingRecord.loadData(observationsRaw.value[id])
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
      pick: ['observationsRaw'],
    },
  }
)
