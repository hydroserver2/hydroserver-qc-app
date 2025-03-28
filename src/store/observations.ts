import { defineStore } from 'pinia'
import { ref } from 'vue'
import { Datastream } from '@/types'
import { fetchObservationsSync } from '@/utils/observationsUtils'
import { ObservationRecord } from '@/utils/plotting/observationRecord'

export const useObservationStore = defineStore(
  'observations',
  () => {
    const observations = ref<Record<string, ObservationRecord>>({}) // TODO: make persistent
    const observationsRaw = ref<
      Record<string, { datetimes: number[]; dataValues: number[] }>
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
        observationsRaw.value[id] = { datetimes: [], dataValues: [] }
        // observationsRaw.value[id].datetimes = []
      }

      if (beginData.dataValues.length > 0) {
        observationsRaw.value[id] = {
          datetimes: [
            ...beginData.datetimes,
            ...observationsRaw.value[id].datetimes,
          ],
          dataValues: [
            ...beginData.dataValues,
            ...observationsRaw.value[id].dataValues,
          ],
        }
      }

      if (endData.dataValues.length > 0) {
        observationsRaw.value[id] = {
          datetimes: [
            ...observationsRaw.value[id].datetimes,
            ...endData.datetimes,
          ],
          dataValues: [
            ...observationsRaw.value[id].dataValues,
            ...endData.dataValues,
          ],
        }
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
        'observationsRaw',
      ],
    },
  }
)
