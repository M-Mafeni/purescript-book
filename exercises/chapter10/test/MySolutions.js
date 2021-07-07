"use strict";

// Note to reader: Add your solutions to this file
const volumeFn = function(w, l, h) {
    return w * l * h;
}

const volumeArrow = function (w) {
   return function(l) {
        return function(h) {
            return w * l * h;
        }
    }
}

const cumulativeSumsComplex = function(complexNums) {
    const addComplexNums = function (c1, c2) {
        return {real: c1.real + c2.real, imag: c1.imag + c2.imag};
    }

    for (let i = 1; i < complexNums.length; i++) {
        complexNums[i] = addComplexNums(complexNums[i], complexNums[i-1]);
    }
    return complexNums;
}
const quadRoots = function (quad) {
    const {a, b, c} = quad;
       const discriminant = (b * b) - (4 * a * c);
       if (discriminant >= 0) {
           const t1 = (-b + Math.sqrt(discriminant)) / (2 * a); 
           const t2 = (-b - Math.sqrt(discriminant)) / (2 * a); 
           return [{real: t1, imag: 0},{real: t2, imag: 0}];
       } else {
           const imag = Math.sqrt(-discriminant)/ (2 * a);
           const real = -b / (2 * a);
           return [{real, imag},{real, imag: -imag}];
       }
}

const quadraticRootsImpl = function(pair) {
    return function (quad) {
        const [v1, v2] = quadRoots(quad);
        return pair(v1)(v2);
    }     
}

const valuesOfMapImpl = function(valuesMap) {
    const tempMap = new Map(valuesMap);
    return Array.from(new Set(tempMap.values()));
}

const quadraticRootsSetImpl = function(quad) {
    return Array.from(new Set(quadRoots(quad)));   
}

exports.volumeFn = volumeFn;
exports.volumeArrow = volumeArrow;
exports.cumulativeSumsComplex = cumulativeSumsComplex
exports.quadraticRootsImpl = quadraticRootsImpl;

exports.unsafeFromUndefined = function(val) {
    if (val !== undefined) {
        return val;
    }
}
exports.valuesOfMapImpl = valuesOfMapImpl;
exports.quadraticRootsSetImpl = quadraticRootsSetImpl;
exports.quadRoots = quadRoots;