import { Datastream } from '@/types'
import { api, getObservationsEndpoint } from '@uwrl/qc-utils'

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
      getObservationsEndpoint(
        id,
        pageSize,
        startTime?.toISOString() ?? phenomenonBeginTime,
        endTime?.toISOString() ?? phenomenonEndTime,
        page,
      )
    )
    page++
  }

  try {
    const results: any[] = []

    for (const endpoint of endpoints) {
      const result = await api.fetchObservations(endpoint)
      if (!result.phenomenonTime.length) {
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
        ...r.phenomenonTime.map((dateString: string) =>
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