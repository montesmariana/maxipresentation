
remark.macros.color = function (hex) {
  const OkabeIto = [
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"]
  const text = this;
  if (parseInt(hex) >= 0) hex = OkabeIto[parseInt(hex)];
  return '<span style="color:' + hex + '">' + text + "</span>";
  };
  
remark.macros.img = function (percentage, alt) {
  var url = this;
  return `<img src="${url}" style ="width:${percentage};" alt="${alt}"/>`;
};