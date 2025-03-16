import { defineStore } from 'pinia'
import { ref } from 'vue'
import { Datastream } from '@/types'
import { fetchObservationsSync } from '@/utils/observationsUtils'
import { ObservationRecord } from '@/utils/plotting/observationRecord'

export const useObservationStore = defineStore('observations', () => {
  const observations = ref<Record<string, ObservationRecord>>({}) // TODO: make persistent

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
    if (!observations.value[id]?.dataset) {
      // TODO: handle error
      const fetchedData = await fetchObservationsSync(
        datastream,
        beginTime,
        endTime
      )

      // Loading the data is an asynchronous operation that cannot be awaited in the constructor
      observations.value[id] = new ObservationRecord(datastream)
      // We implicitly call `loadData` here to await it
      await observations.value[id].loadData(fetchedData)
      // observations.value[id].generateDataset()

      // Return the ObservationRecord
      return observations.value[id]
    } else {
      const existingRecord = observations.value[id]
      let beginDataPromise: Promise<any[]> = Promise.resolve([])
      let endDataPromise: Promise<any[]> = Promise.resolve([])

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

        endDataPromise = fetchObservationsSync(
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
        observations.value[id].dataArray = [
          ...beginData,
          ...observations.value[id].dataArray,
        ]
      }

      if (endData.length > 0) {
        observations.value[id].dataArray = [
          ...observations.value[id].dataArray,
          ...endData,
        ]
      }

      // If the data has changed, renegerate the dataset
      // if (beginData.length > 0 || endData.length > 0) {
      //   existingRecord.generateDataset()
      // }

      existingRecord.isLoading = false

      // Return only the data within the requested range
      // TODO: set a data frame date filter using beginTime and endTime
      // TODO: add to current filter in store
      // observations.value[id].dataFrame.set_filter({
      //   [FilterOperation.START]: beginTime.getTime(),
      //   [FilterOperation.END]: endTime.getTime(),
      // })
      return observations.value[id]
    }
  }

  return {
    observations,
    fetchObservationsInRange,
  }
})
