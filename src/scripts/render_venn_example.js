import * as d3 from 'd3-selection';
import * as venn from 'venn.js';

const sets = [
  {sets: ['Zeitspendenplattform'], size: 13},
  {sets: ['Sachspendenplattform'], size: 9},
  {sets: ['Zeitspendenplattform', 'Sachspendenplattform'], size: 8}
];

const chart = venn.VennDiagram();
d3.select('#venn').datum(sets).call(chart);
