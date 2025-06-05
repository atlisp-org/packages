/**
 * 平面n个点的最短连线算法----最短路径排序
 * 平面内有多个点，要用一根线逐个连接所有点，每个点不重复。返回排序好的点数组
 * @param pathArray 平面内有点坐标数组
 */
function shortPathRank (pathArray) {
    var newArray = [] // 排序好的点数组
    if (pathArray.length > 2) { // 只有点数大于2排序才有意义
                                var pathLengths = [] // 所用组合构成的路线长度数组
    var newArrays = [] // 所有组合构成的点排序好的数组
    var changeArray = [] // 用来做排序用的改变点数组
    var minPoint // 用来比较那个点距离最短的中间值
    // 循环改变计算的第一个点
    for (let z = 0; z < pathArray.length; z++) {
      newArray = []
      newArray.push(pathArray[z]) // 赋值路线的起始点
      // 嵌套循环计算其他点与起始点的最短距离的点
      for (let i = 0; i < pathArray.length - 2; i++) {
        changeArray = []
        // 初始化改变数组---将已经排序过的点排除
        pathArray.forEach(item => {
          if (!newArray.includes(item)) {
            changeArray.push(item)
          }
        })
        minPoint = changeArray[0] // 为最小距离点赋初始值
        // 循环比较排序好的最后一个点与其他没有排序点的距离
        for (let j = 0; j < changeArray.length - 1; j++) {
          if (dist(newArray[i], minPoint) > dist(newArray[i], changeArray[j + 1])) {
            minPoint = changeArray[j + 1]
          }
        }
        // 比较完后得到最短的那个点加到排序的点数组中
        newArray.push(minPoint)
      }
      // 改变的点数组中剩下最后一个点，不需要比较，直接去重加到排序数组中
      pathArray.forEach(item => {
        if (!newArray.includes(item)) {
          newArray.push(item)
        }
      })
      // 将这种点排列路径长度计算出来加到线长度数组中
      pathLengths.push(totalDist(newArray))
      // 将这种点排列情况加到点排序好的数组
      newArrays.push(newArray)
    }
    // 计算出线长度数组中路线最短的那个可能的下标去点排序好的数组取对应的点排序可能
    newArray = newArrays[getMinIndex(pathLengths)]
  } else {
    newArray = [...pathArray]
  }
  return newArray
}
 
// 求两点之间距离
function dist (p1, p2) {
  var a = p2[0] - p1[0]
  var b = p2[1] - p1[1]
  return Math.sqrt(a * a + b * b)
}
 
// 求所有点连线的总长度
function totalDist (pointArray) {
  var totalLength = 0
  for (let i = 0; i < pointArray.length - 1; i++) {
    totalLength += dist(pointArray[i], pointArray[i + 1])
  }
  return totalLength
}
 
// 求数组中最小值的下标
function getMinIndex (arr) {
  var min = arr[0]
  // 声明了个变量 保存下标值
  var index = 0
  for (var i = 0; i < arr.length; i++) {
    if (min > arr[i]) {
      min = arr[i]
      index = i
    }
  }
  return index
}
