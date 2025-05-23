import { DataArray, DataPoint, Datastream, TimeSpacingUnit } from '@/types'
import { api, getObservationsEndpoint } from '@/services/api'

export function subtractHours(timestamp: string, hours: number): string {
  const date = new Date(timestamp)
  date.setHours(date.getHours() - hours)
  return date.toISOString()
}

export const fetchObservationsSync = async (
  datastream: Datastream,
  startTime?: Date,
  endTime?: Date
): Promise<{ datetimes: number[]; dataValues: number[] }> => {
  const { id, phenomenonBeginTime, phenomenonEndTime, valueCount } = datastream
  if (!phenomenonBeginTime || !phenomenonEndTime) {
    return { datetimes: [], dataValues: [] }
  }

  const pageSize = 50_000
  const endpoints: string[] = []
  let page = 1
  const maxPages = Math.ceil(valueCount / pageSize)
  while (page <= maxPages) {
    endpoints.push(
      getObservationsEndpoint({
        id,
        pageSize,
        startTime: startTime?.toISOString() ?? phenomenonBeginTime,
        endTime: endTime?.toISOString() ?? phenomenonEndTime,
        page,
        addResultQualifiers: true,
      })
    )
    page++
  }

  try {
    const results: any[] = []

    for (const endpoint of endpoints) {
      const result = await api.fetchObservations(endpoint)
      if (!result.phenomenon_time.length) {
        break
      }
      results.push(result)
    }

    // return (
    //   results
    //     // TODO: Unsertain how to map result qualifiers. Ommiting for now.

    //     .map(
    //       (r) =>
    //         r.value[0]?.dataArray.map((row: [string, number, any]) => [
    //           row[0],
    //           row[1],
    //         ]) || []
    //     )
    //     .flat()
    // )

    let datetimes: number[] = []
    let dataValues: number[] = []

    results.forEach((r) => {
      datetimes = [
        ...datetimes,
        ...r.phenomenon_time.map((dateString: string) =>
          new Date(dateString).getTime()
        ),
      ]
      dataValues = [...dataValues, ...r.result]
    })

    return {
      datetimes,
      dataValues,
    }
  } catch (error) {
    console.error('Error fetching data:', error)
    return Promise.reject(error)
  }
}

// export function toDataPointArray(dataArray: DataArray): DataPoint[] {
//   return dataArray.map(([dateString, value, qualifiers]) => ({
//     date: new Date(dateString),
//     value,
//     qualifierValue: qualifiers.resultQualifiers.map((q) => q.code),
//   }))
// }

// Function to replace 'no data' values with NaN
// export function replaceNoDataValues(data: DataPoint[], noDataValue: number) {
//   return data.map((d) => ({
//     ...d,
//     value: d.value === noDataValue ? NaN : d.value,
//   }))
// }

// export function convertTimeSpacingToMilliseconds(
//   timeSpacing: number,
//   unit: TimeSpacingUnit
// ): number {
//   const unitToMilliseconds = {
//     seconds: 1000,
//     minutes: 1000 * 60,
//     hours: 1000 * 60 * 60,
//     days: 1000 * 60 * 60 * 24,
//   }

//   return timeSpacing * (unitToMilliseconds[unit] || 0)
// }

// function calculateTimeDifference(point1: DataPoint, point2: DataPoint): number {
//   const time1 = new Date(point1.date).getTime()
//   const time2 = new Date(point2.date).getTime()

//   return Math.abs(time2 - time1)
// }

// export function addNaNForGaps(data: DataPoint[], maxGap: number): DataPoint[] {
//   const modifiedData: DataPoint[] = []
//   data.forEach((point, index) => {
//     modifiedData.push(point)
//     if (index < data.length - 1) {
//       const timeDifference = calculateTimeDifference(point, data[index + 1])
//       if (timeDifference > maxGap) {
//         modifiedData.push({
//           date: new Date(point.date.getTime() + 1),
//           value: NaN,
//           qualifierValue: [],
//         })
//       }
//     }
//   })
//   return modifiedData
// }

// export function preProcessData(dataArray: DataArray, datastream: Datastream) {
//   const { noDataValue, intendedTimeSpacing, intendedTimeSpacingUnits } =
//     datastream

//   // TODO: avoid extra iterations and perform all transformations in one
//   let data = toDataPointArray(dataArray)
//   data = replaceNoDataValues(data, noDataValue)

//   if (intendedTimeSpacingUnits && intendedTimeSpacing) {
//     const maxGap = convertTimeSpacingToMilliseconds(
//       intendedTimeSpacing,
//       intendedTimeSpacingUnits as TimeSpacingUnit
//     )

//     data = addNaNForGaps(data, maxGap)
//   }
//   return data
// }
