//   /**
//    * Large data down sampling using largest-triangle-three-buckets
//    * @param {string} valueDimension
//    * @param {number} targetCount
//    */
//   lttbDownSample(
//     valueDimension: DimensionIndex,
//     rate: number
// ): DataStore {
//     const target = this.clone([valueDimension], true);
//     const targetStorage = target._chunks;
//     const dimStore = targetStorage[valueDimension];
//     const len = this.count();

//     let sampledIndex = 0;

//     const frameSize = Math.floor(1 / rate);

//     let currentRawIndex = this.getRawIndex(0);
//     let maxArea;
//     let area;
//     let nextRawIndex;

//     const newIndices = new (getIndicesCtor(this._rawCount))(Math.min((Math.ceil(len / frameSize) + 2) * 2, len));

//     // First frame use the first data.
//     newIndices[sampledIndex++] = currentRawIndex;
//     for (let i = 1; i < len - 1; i += frameSize) {
//         const nextFrameStart = Math.min(i + frameSize, len - 1);
//         const nextFrameEnd = Math.min(i + frameSize * 2, len);

//         const avgX = (nextFrameEnd + nextFrameStart) / 2;
//         let avgY = 0;

//         for (let idx = nextFrameStart; idx < nextFrameEnd; idx++) {
//             const rawIndex = this.getRawIndex(idx);
//             const y = dimStore[rawIndex] as number;
//             if (isNaN(y)) {
//                 continue;
//             }
//             avgY += y as number;
//         }
//         avgY /= (nextFrameEnd - nextFrameStart);

//         const frameStart = i;
//         const frameEnd = Math.min(i + frameSize, len);

//         const pointAX = i - 1;
//         const pointAY = dimStore[currentRawIndex] as number;

//         maxArea = -1;

//         nextRawIndex = frameStart;

//         let firstNaNIndex = -1;
//         let countNaN = 0;
//         // Find a point from current frame that construct a triangle with largest area with previous selected point
//         // And the average of next frame.
//         for (let idx = frameStart; idx < frameEnd; idx++) {
//             const rawIndex = this.getRawIndex(idx);
//             const y = dimStore[rawIndex] as number;
//             if (isNaN(y)) {
//                 countNaN++;
//                 if (firstNaNIndex < 0) {
//                     firstNaNIndex = rawIndex;
//                 }
//                 continue;
//             }
//             // Calculate triangle area over three buckets
//             area = Math.abs((pointAX - avgX) * (y - pointAY)
//                 - (pointAX - idx) * (avgY - pointAY)
//             );
//             if (area > maxArea) {
//                 maxArea = area;
//                 nextRawIndex = rawIndex; // Next a is this b
//             }
//         }

//         if (countNaN > 0 && countNaN < frameEnd - frameStart) {
//             // Append first NaN point in every bucket.
//             // It is necessary to ensure the correct order of indices.
//             newIndices[sampledIndex++] = Math.min(firstNaNIndex, nextRawIndex);
//             nextRawIndex = Math.max(firstNaNIndex, nextRawIndex);
//         }

//         newIndices[sampledIndex++] = nextRawIndex;

//         currentRawIndex = nextRawIndex; // This a is the next a (chosen b)
//     }

//     // First frame use the last data.
//     newIndices[sampledIndex++] = this.getRawIndex(len - 1);
//     target._count = sampledIndex;
//     target._indices = newIndices;

//     target.getRawIndex = this._getRawIdx;
//     return target;
// }
