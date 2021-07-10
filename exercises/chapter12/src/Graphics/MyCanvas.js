exports.setGradientStrokeStyle = (ctx) => (gradient) => () => {
  ctx.strokeStyle = gradient;
}