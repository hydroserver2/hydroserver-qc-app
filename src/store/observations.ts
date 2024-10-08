import { defineStore } from 'pinia'
import { ref } from 'vue'
import { Datastream, ObservationRecord } from '@/types'
import { fetchObservationsParallel } from '@/utils/observationsUtils'
import { FilterOperation, usePyStore } from '@/store/py'

export const useObservationStore = defineStore('observations', () => {
  const observations = ref<Record<string, ObservationRecord>>({})

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

    // If nothing is stored yet, create a new record and fetch the data in range
    if (!observations.value[id]?.dataFrame) {
      // TODO: handle error
      const fetchedData = await fetchObservationsParallel(
        datastream,
        beginTime,
        endTime
      )

      observations.value[id] = new ObservationRecord(fetchedData)

      // Return the dataframe
      return observations.value[id]
    } else {
      const existingRecord = observations.value[id]
      // const newBeginTime = new Date(beginTime).getTime()
      // const newEndTime = new Date(endTime).getTime()
      // const storedBeginTime = new Date(existingRecord.beginTime).getTime()
      // const storedEndTime = new Date(existingRecord.endTime).getTime()

      let beginDataPromise: Promise<any[]> = Promise.resolve([])
      let endDataPromise: Promise<any[]> = Promise.resolve([])

      // Check if new data before the stored data is needed
      if (beginTime < existingRecord.beginTime) {
        beginDataPromise = fetchObservationsParallel(
          datastream,
          beginTime,
          existingRecord.beginTime
        )
      }

      // Check if new data after the stored data is needed
      if (endTime > existingRecord.endTime) {
        const temp = existingRecord.endTime
        temp.setSeconds(temp.getSeconds() + 1)

        endDataPromise = fetchObservationsParallel(
          datastream,
          // TODO: is inclusive. We need to increase by one second
          temp,
          endTime
        )
      }

      // Fetch and update in parallel if needed
      const [beginData, endData] = await Promise.all([
        beginDataPromise,
        endDataPromise,
      ])

      if (beginData.length > 0) {
        observations.value[id].dataFrame.add_points(beginData)
      }

      if (endData.length > 0) {
        observations.value[id].dataFrame.add_points(endData)
      }

      existingRecord.isLoading = false

      // Return only the data within the requested range
      // TODO: set a data frame date filter using beginTime and endTime
      // TODO: add to current filter in store
      observations.value[id].dataFrame.set_filter({
        [FilterOperation.START]: beginTime.getTime(),
        [FilterOperation.END]: endTime.getTime(),
      })
      return observations.value[id]
    }
  }

  return {
    observations,
    fetchObservationsInRange,
  }
})
