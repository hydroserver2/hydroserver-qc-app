import { DimensionIndex, SeriesOption } from 'echarts/types/src/util/types'
import {
  StageHandler,
  SeriesSamplingOptionMixin,
} from 'echarts/types/src/util/types'
import SeriesModel from 'echarts/types/src/model/Series'
import { range } from './interpolate'
import Point from 'zrender/lib/core/Point'

/**
 * Large data down sampling
 * @param {string} valueDimension
 * @param params
 */
function customDownSample(valueDimension: DimensionIndex, params: any) {
  // TODO:  IN PROGRESS
  console.log('customDownSample')
  const xAxis = params[0]
  const yAxis = params[1]
  // const api = params[2]

  const target = this.clone([valueDimension], true)
  const targetStorage = target._chunks
  const yStore = targetStorage[valueDimension]
  const xStore = targetStorage[0]

  const len = this.count()

  const xExtent = xAxis.getExtent()
  const rawXExtent = xAxis.scale.rawExtentInfo
  const xDomain = [
    rawXExtent._determinedMin || rawXExtent._dataMin,
    rawXExtent._determinedMax || rawXExtent._dataMax,
  ]

  const yExtent = yAxis.getExtent()
  const yDomain = this.getDataExtent(1)

  // const dpr = api.getDevicePixelRatio()
  // In case coordinste system has been resized
  // const size = Math.abs(xExtent[1] - xExtent[0]) * (dpr || 1)

  const minDistance = 1 // TODO: compute from symbolsize

  const getDataPointCoords = (rawIndex: number): Point => {
    const x = range(
      xDomain[0],
      xDomain[1],
      xExtent[0],
      xExtent[1],
      xStore[rawIndex]
    )
    const y = range(
      yDomain[0],
      yDomain[1],
      yExtent[0],
      yExtent[1],
      yStore[rawIndex]
    )

    return new Point(x, y)
  }

  let a = 0
  let b = 1
  let c = 2

  target._indices = [0] // We always want the first and last point

  /*
   * Calculates the angle ABC (in degrees)
   */
  const findAngle = (a: Point, b: Point, c: Point) => {
    const AB = Math.sqrt(Math.pow(b.x - a.x, 2) + Math.pow(b.y - a.y, 2))
    const BC = Math.sqrt(Math.pow(b.x - c.x, 2) + Math.pow(b.y - c.y, 2))
    const AC = Math.sqrt(Math.pow(c.x - a.x, 2) + Math.pow(c.y - a.y, 2))
    const radians = Math.acos((BC * BC + AB * AB - AC * AC) / (2 * BC * AB))
    const degrees = (radians * 180) / Math.PI
    return degrees
  }

  /**
   * This algorithm aims to produce the same series shape and minimize the number of points drawn that endup overlapped by some other point.
   * It is important to preserve the shape of the series (i.e.: Not to inflection points or outliers)
   * We can ignore a point if any of these conditions apply:
   *  a. Tt is too close to either neighbor.
   *  b. It forms a straight line with its neighbors and some other point overlaps it.
   *
   * TODO: not seeing good accuracy + performance from any down sampling algorithm.
   * @see https://stackoverflow.com/a/78015099
   */

  const areStraightLine = (a: Point, b: Point, c: Point): boolean => {
    const angle = Math.abs(findAngle(a, b, c))
    const delta = 5
    return angle > 180 - delta && angle < 180 + delta
  }

  while (c <= len - 1) {
    // 'i' is the index in the current visible data, but we need the raw index
    // Get the pixel coordinates of the current data point
    const pointA = getDataPointCoords(this.getRawIndex(a))
    const pointB = getDataPointCoords(this.getRawIndex(b))
    const pointC = getDataPointCoords(this.getRawIndex(c))

    if (
      pointB.distance(pointA) < minDistance ||
      pointB.distance(pointC) < minDistance
    ) {
      // We can ignore b
    }
    // Check if part of a straight line
    else if (areStraightLine(pointA, pointB, pointC)) {
      // TODO: Do not ignore if the point has no near neighbors
    } else {
      // We cannot ignore b
      a = b
      target._indices.push(this.getRawIndex(b))
    }
    b++
    c++
  }

  target._indices.push(this.getRawIndex(len - 1)) // Append the last point
  target._count = target._indices.length // Where to slice the _indices array

  console.log('Total Points: ' + len)
  console.log('After downsampling: ' + target._count)
  console.log(target._indices)

  target.getRawIndex = this._getRawIdx
  return target
}

export default function dataSample(seriesType: string): StageHandler {
  return {
    seriesType: seriesType,

    reset: function (
      seriesModel: SeriesModel<SeriesOption & SeriesSamplingOptionMixin>,
      _ecModel,
      api
    ) {
      const data = seriesModel.getData()
      const sampling = seriesModel.get('sampling')
      const coordSys = seriesModel.coordinateSystem
      const count = data.count()
      // Only cartesian2d support down sampling. Disable it when there is few data.
      if (count > 10 && coordSys.type === 'cartesian2d' && sampling) {
        // @ts-ignore
        const baseAxis = coordSys.getBaseAxis()
        // @ts-ignore
        const valueAxis = coordSys.getOtherAxis(baseAxis)

        // @ts-ignore
        if (sampling === 'custom-lttb') {
          // @ts-ignore: Hijack the internal lttbDownSample method and provide our own
          data._store.lttbDownSample = customDownSample
          seriesModel.setData(
            // @ts-ignore
            data.lttbDownSample(data.mapDimension(valueAxis.dim), [
              baseAxis,
              valueAxis,
              api,
            ])
          )
        }
      }
    },
  }
}
