// import { SeriesOption } from 'echarts/types/src/util/types'
// import SeriesModel from 'echarts/types/src/model/Series'
// import {
//   StageHandler,
//   SeriesSamplingOptionMixin,
// } from 'echarts/types/src/util/types'

// export default function dataSample(seriesType: string): StageHandler {
//   return {
//     seriesType: seriesType,

//     reset: function (
//       seriesModel: SeriesModel<SeriesOption & SeriesSamplingOptionMixin>,
//       ecModel,
//       api
//     ) {
//       const data = seriesModel.getData()
//       const sampling = seriesModel.get('sampling')
//       console.log(sampling)
//       const coordSys = seriesModel.coordinateSystem
//       const count = data.count()
//       // Only cartesian2d support down sampling. Disable it when there is few data.
//       if (count > 10 && coordSys.type === 'cartesian2d' && sampling) {
//         const baseAxis = coordSys.getBaseAxis()
//         const valueAxis = coordSys.getOtherAxis(baseAxis)
//         const extent = baseAxis.getExtent()
//         const dpr = api.getDevicePixelRatio()
//         // Coordinste system has been resized
//         const size = Math.abs(extent[1] - extent[0]) * (dpr || 1)
//         const rate = Math.round(count / size)

//         if (isFinite(rate) && rate > 1) {
//           if (sampling === 'custom-lttb') {
//             seriesModel.setData(
//               data.lttbDownSample(data.mapDimension(valueAxis.dim), 1 / rate)
//             )
//           }
//         }
//       }
//     },
//   }
// }
