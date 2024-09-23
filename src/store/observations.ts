import { defineStore } from 'pinia'
import { ref } from 'vue'
import { Datastream, ObservationRecord } from '@/types'
import { fetchObservationsParallel } from '@/utils/observationsUtils'
import { usePyStore } from '@/store/py'

const { instantiateDataFrame } = usePyStore()

export const useObservationStore = defineStore('observations', () => {
  const observations = ref<Record<string, ObservationRecord>>({})

  /**
   * Fetches requested observations that aren't currently in the pinia store,
   * updates the store, then returns the requested observations.
   * TODO: this method performs too many iterations over the dataset
   */
  const fetchObservationsInRange = async (
    datastream: Datastream,
    beginTime: string,
    endTime: string
  ): Promise<ObservationRecord> => {
    const id = datastream.id

    // If nothing is stored yet, create a new record and fetch the data in range
    if (!observations.value[id]?.dataFrame) {
      // TODO: handle error
      const fetchedData = await fetchObservationsParallel(
        datastream,
        beginTime,
        endTime
      )

      const dataFrame = instantiateDataFrame(
        JSON.stringify({
          dataArray: fetchedData,
          components: ['date', 'value', 'qualifierCode'],
        })
      )

      observations.value[id] = new ObservationRecord(dataFrame)

      // TODO: return from data frame
      // Return the entire dataframe
      return observations.value[id]
    } else {
      const existingRecord = observations.value[id]
      const newBeginTime = new Date(beginTime).getTime()
      const newEndTime = new Date(endTime).getTime()
      const storedBeginTime = new Date(existingRecord.beginTime).getTime()
      const storedEndTime = new Date(existingRecord.endTime).getTime()

      let beginDataPromise = Promise.resolve([])
      let endDataPromise = Promise.resolve([])

      // Check if new data before the stored data is needed
      if (newBeginTime < storedBeginTime) {
        beginDataPromise = fetchObservationsParallel(
          datastream,
          beginTime,
          existingRecord.beginTime
        )
      }

      // Check if new data after the stored data is needed
      if (newEndTime > storedEndTime) {
        endDataPromise = fetchObservationsParallel(
          datastream,
          existingRecord.endTime,
          endTime
        )
      }

      // Fetch and update in parallel if needed
      const [beginData, endData] = await Promise.all([
        beginDataPromise,
        endDataPromise,
      ])

      // TODO: append and prepend to data frame
      if (beginData.length > 0) {
        JSON.stringify(beginData)
        observations.value[id].dataFrame.add_points(beginData)
        // existingRecord.dataArray = [...beginData, ...existingRecord.dataArray]
        existingRecord.beginTime = beginTime
      }

      if (endData.length > 0) {
        observations.value[id].dataFrame.add_points(endData)
        // existingRecord.dataArray = [...existingRecord.dataArray, ...endData]
        existingRecord.endTime = endTime
      }

      existingRecord.loading = false

      // Return only the data within the requested range
      // TODO: return from dataframe
      // TODO: set a dataframe date filter using beginTime and endTime
      return observations.value[id]

      return observations.value[id].dataArray.filter(([dateString, _]) => {
        const observationTimestamp = new Date(dateString).getTime()
        return (
          observationTimestamp >= newBeginTime &&
          observationTimestamp <= newEndTime
        )
      })
    }
  }

  return {
    observations,
    fetchObservationsInRange,
  }
})
