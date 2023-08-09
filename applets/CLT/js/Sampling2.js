"use strict";


/**
 * @constructor
 */
var Sampling = function() {
};


Sampling.numberOfBins = 33;


/**
 * @returns {Array} An array of the bin values used for displaying all stats other than variance.
 */
Sampling.integerValues = function() {
  return Sampling.valuesWithInterval(1);
};


/**
 * @returns {Array} An array of the bin values used displaying variance.
 */
Sampling.valuesWithInterval = function(interval) {
    var values = [];
    for (var i = 0; i < Sampling.numberOfBins; i++) {
        values.push(i * interval);
    }
    return values;
};


Sampling.normalFrequencies = function() {
    return [2,3,3,6,8,14,19,32,45,60,78,97,116,133,147,156,160,156,147,133,116,97,78,60,45,32,19,14,8,6,3,3,2];
};


Sampling.skewedFrequencies = function (values) {
    var numberOfBins = Sampling.numberOfBins;
    var s = numberOfBins / 6;
    var mn = (numberOfBins - 1) / 2;
    var temp =  Math.sqrt(1.0 / (2.0 * Math.PI * s * s));
    var skewMax = Math.pow( values[numberOfBins-1], 2);
    var skewedData = [];

    for (var i = 0; i < numberOfBins; i++){
        var z = -1 * Math.pow((values[i] - mn), 2) / (2*s*s);
        z = Math.exp(z) * temp;
        skewedData.push(Math.round(Math.pow(values[numberOfBins-i-1],2)/skewMax * 5000));
    }
  	skewedData[0]=skewedData[6];
  	skewedData[1]=skewedData[5];
  	skewedData[2]=skewedData[4];
    return skewedData;
};


Sampling.uniformFrequencies = function(constant) {
    var result = [];
    for (var i = 0; i < Sampling.numberOfBins; i++) {
        result.push(constant);
    }
    return result;
};


Sampling.normalDistribution = function() {
    return new HistogramData(Sampling.integerValues(), Sampling.normalFrequencies());
};


Sampling.skewedDistribution = function() {
    var values = Sampling.integerValues();
    return new HistogramData(values, Sampling.skewedFrequencies(values));
};


Sampling.uniformDistribution = function() {
    return new HistogramData(Sampling.integerValues(), Sampling.uniformFrequencies(15));
};


Sampling.prototype.setupHistograms = function() {
    var self = this;
    // Create a histogram in the div ID histogram1
    var histogram1 = new Histogram($('div #histogram1'),
        '',
        Sampling.normalDistribution(),
        false, /* yTicks */
        0,     /* yIntervals */
        true,  /* shouldPlotStats */
        true   /* editable */);
    this.histogram1 = histogram1;
    // Set up control

    var changeStats = function(histogram) {
        // Choose the stats display object.
        // It would be cleaner if we associated this with the actual histogram object, this would be good to refactor
        var histogramID = '1';
        if (histogram === self.histogram2) {
          histogramID = '2';
        }
        else if (histogram === self.histogram3) {
          histogramID = '3';
        }
        else if (histogram === self.histogram4) {
          histogramID = '4';
        }
        var statsDisplay = '#statsDisplay' + histogramID;
        if (histogram.histogramData.numberOfObservations() > 1) {
            $(statsDisplay).show();
            var statistics = histogram.histogramData.getStatistics();
            $('#reps' + histogramID).html(statistics.numberOfObservations.toFixed(0));
            $('#range' + histogramID).html(statistics.formatValue(statistics.range));
            $('#mean' + histogramID).html(statistics.formatValue(statistics.mean));
            $('#median' + histogramID).html(statistics.formatValue(statistics.median));
            $('#sd' + histogramID).html(statistics.formatValue(statistics.sd));
            $('#skew' + histogramID).html(statistics.formatValue(statistics.skew));
            $('#kurtosis' + histogramID).html(statistics.formatValue(statistics.kurtosis));
        }
        else {
            // Hide statistics display because there is no data
            $(statsDisplay).hide();
        }
    };
    histogram1.addObserver(changeStats, 'changeStats');
    changeStats(histogram1);

    // Set up animated sample data histogram
    var histogram2 = new AnimatedHistogram($('div #histogram2'),
        '',
        new HistogramData(Sampling.integerValues(), Sampling.uniformFrequencies(0)),
        true /* yTicks */,
        6,
        false,  /* shouldPlotStats */
        false   /* editable */);
    this.histogram2 = histogram2;

    histogram2.addObserver(changeStats, 'changeStats');
    changeStats(histogram2);

    // Set up sample data histogram
    var histogram3 = new AnimatedHistogram($('div #histogram3'),
        '',
        new HistogramData(Sampling.integerValues(), Sampling.uniformFrequencies(0)),
        true /* yTicks */,
        6,    /* yIntervals */
        true,  /* shouldPlotStats */
        false   /* editable */);
    this.histogram3 = histogram3;

    histogram3.addObserver(changeStats, 'changeStats');
    changeStats(histogram3);

    // Set up sample data histogram
    var histogram4 = new AnimatedHistogram($('div #histogram4'),
        '',
        new HistogramData(Sampling.integerValues(), Sampling.uniformFrequencies(0)),
        true,/* yTicks */
        6,    /* yIntervals */
        true,  /* shouldPlotStats */
        false   /* editable */);
    this.histogram4 = histogram4;
    histogram4.addObserver(changeStats, 'changeStats');
    changeStats(histogram4);
};


Sampling.prototype.setupControls = function() {
    var self = this;
    // Set up 'clear lower 3' button
    $('#resetSamples').click(function() {
      self.resetSamples();
    });

    // Set up control to change distribution
    var histogram1 = this.histogram1;
    var $selectDistribution = $('select[name=distribution]');
    $selectDistribution.change(function() {
        var val = $(this).val();
        if (val === "Normal") {
            histogram1.setHistogramData(Sampling.normalDistribution());
        }
        else if (val === "Uniform") {
            histogram1.setHistogramData(Sampling.uniformDistribution());
        }
        else if (val === "Skewed") {
            histogram1.setHistogramData(Sampling.skewedDistribution());
        }
        else if (val === "Custom") {
            histogram1.setHistogramData(new HistogramData(Sampling.integerValues(), Sampling.uniformFrequencies(0)));
        }
        if (val !== self.currentDistribution) {
          self.resetSamples();
        }
        self.currentDistribution = val;
    });

    $('#sample5').click(function() {
        self.updateChartsWithSamples(5);
    });
    $('#sample10000').click(function() {
        self.updateChartsWithSamples(10000);
    });
    $('#sample100000').click(function() {
        self.updateChartsWithSamples(100000);
    });

    $('#sampleAnimated').click(function() {
      self.sampleAnimated();
    });

  var histogram3Changed = function() {
    var statToDisplay = $('#histogram3Stat :selected').text();
    var selectedStat = $('#histogram3Stat :selected').val();
    // var sampleSize = $('#histogram3SampleSize :selected').text();
    var sampleSize = $('#histogram3SampleSize').val();

    document.getElementById("my3titleId").innerHTML = 'Distribution of ' + statToDisplay + 's, ' + sampleSize;

    self.histogram3.setBarColor(self.colorForStat(selectedStat));
    self.resetSamples();
  };
  $('#histogram3Stat, #histogram3SampleSize').change(histogram3Changed);
  histogram3Changed();

  $('#histogram4Stat, #histogram4SampleSize').change(function() {
    var statToDisplay = $('#histogram4Stat :selected').text();
    var selectedStat = $('#histogram4Stat :selected').val();
    // var sampleSize = $('#histogram4SampleSize :selected').text();
    var sampleSize = $('#histogram3SampleSize').val();
    
    if (selectedStat === 'none') {
      self.histogram4.setTitle('');
    }
    else {
      self.histogram4.setTitle('Distribution of ' + statToDisplay + 's, ' + sampleSize);
    }
    self.histogram4.setBarColor(self.colorForStat(selectedStat));
    self.resetSamples();
  });

  $('#histogram3FitNormal').change(function() {
    var shouldFitNormal = this.checked;
    self.histogram3.setFitNormal(shouldFitNormal);
  });

  $('#histogram4FitNormal').change(function() {
    var shouldFitNormal = this.checked;
    self.histogram4.setFitNormal(shouldFitNormal);
  });
};


/**
 * Reset samples.  Called whenever the user changes sample size or statistic display.
 */
Sampling.prototype.resetSamples = function() {
  var selectedStat3 = $('#histogram3Stat :selected').val();
  // var sampleSize3 = $('#histogram3SampleSize :selected').val();
  var sampleSize3 = $('#histogram3SampleSize').val();
  var selectedStat4 = $('#histogram4Stat :selected').val();
  var sampleSize4 = $('#histogram4SampleSize :selected').val();
  this.histogram2.setHistogramData(HistogramData.makeEmpty(Sampling.integerValues()));
  this.histogram3.setHistogramData(HistogramData.makeEmpty(this.binValuesForStat(selectedStat3, parseInt(sampleSize3))));
  this.histogram4.setHistogramData(HistogramData.makeEmpty(this.binValuesForStat(selectedStat4, parseInt(sampleSize4))));
};


/**
 * Return the histogram bin interval to use for the specified statistic.
 * @param {string} statName The name of the stat being displayed.
 * @param {number} sampleSize The number of samples.
 * @return {number} The interval between bins in the histogram.
 */
Sampling.prototype.valueIntervalForStat = function(statName, sampleSize)
{
  var distribution = $('select[name=distribution]').val();

  if (statName === 'variance' || statName === 'varianceUnbiased') {
    // Use different scales to show distributions of variances
    if (distribution === 'Custom') {
      return 8;
    }
    else if (distribution === 'Normal') {
      switch (sampleSize) {
        case 2:
          return 6;
        case 5:
          return 4;
        case 10:
          return 3;
        case 16:
          return 3;
        case 20:
          return 2;
        case 25:
          return 2;
        default:
          return 1;
      }
    }
    else if (distribution === 'Skewed') {
      switch (sampleSize) {
        case 2:
          return 8;
        case 5:
          return 8;
        case 10:
          return 6;
        case 16:
          return 6;
        case 20:
          return 5;
        case 25:
          return 5;
        default:
          return 1;
      }
    }
  }
  return 1;
};


/**
 * Return the histogram bin values to use for the specified statistic
 * @param {string} statName The name of the stat being displayed.
 * @param {number} sampleSize The sample size.
 * @return {Array<number>} The bin values to use to display the specified stat.
 */
Sampling.prototype.binValuesForStat = function(statName, sampleSize) {
  var interval = this.valueIntervalForStat(statName, sampleSize);
  return Sampling.valuesWithInterval(interval);
};


/**
 * Return the color to use for the specified statistic.
 * @param statName The name of the statistic.
 */
Sampling.prototype.colorForStat = function(statName) {
  if (statName === 'mean') {
    return 'blue';
  }
  else if (statName === 'median') {
    return 'magenta';
  }
  else if (statName === 'standardDeviation') {
    return 'red';
  }
  else if (statName === 'variance') {
    return '#404040'; // dark gray
  }
  else if (statName === 'varianceUnbiased') {
    return 'silver'; // light gray
  }
  else if (statName === 'meanAbsoluteDeviation') {
    return 'cyan';
  }
  else if (statName === 'range') {
    return 'lime';
  }
  else {
    return '#404040'; // dark gray
  }
};


/**
 * Update both charts, if necessary.  Display the distribution after taking a number of random samples.
 * @param numberOfSamples The number of samples.
 */
Sampling.prototype.updateChartsWithSamples = function(numberOfSamples) {
    this.histogram2.setHistogramData(HistogramData.makeEmpty(Sampling.integerValues()));
    // var sampleSize1 = $('#histogram3SampleSize :selected').val();
    var sampleSize1 = $('#histogram3SampleSize').val();

    var stat1 = $('#histogram3Stat :selected').val();
    this.updateChart(this.histogram3, parseInt(sampleSize1), numberOfSamples, stat1);
    this.histogram3.setBarColor(this.colorForStat(stat1));

    var sampleSize2 = $('#histogram4SampleSize :selected').val();
    var stat2 = $('#histogram4Stat :selected').val();
    this.updateChart(this.histogram4, parseInt(sampleSize2), numberOfSamples, stat2);
    this.histogram4.setBarColor(this.colorForStat(stat2));
};


/**
 * @param histogram The histogram to update.
 * @param sampleSize
 * @param numberOfSamples
 * @param statToDisplay
 */
Sampling.prototype.updateChart = function(histogram, sampleSize, numberOfSamples, statToDisplay) {
    var parentPopulation = this.histogram1.histogramData;
    if (statToDisplay == 'none') {
        histogram.setHistogramData(HistogramData.makeEmpty(parentPopulation.values));
        histogram.setTitle('');
    }
    else {
        var reduceFunction = StatisticsFunctions.functionByName(statToDisplay);
        var samples = this.sampleMany(parentPopulation, sampleSize, numberOfSamples, reduceFunction);
        var newSamples = HistogramData.makeWithDataPoints(samples, histogram.histogramData.values);
        var existingSamples = histogram.histogramData;
        var allSamples = HistogramData.makeByCombiningData(existingSamples, newSamples);
        if (allSamples.numberOfObservations() !== newSamples.numberOfObservations() + existingSamples.numberOfObservations()) {
          console.log('ruh roh');
        }
        histogram.setHistogramData(allSamples);
    }
};


/**
 * Generate a random sample from a distribution.
 * @param {HistogramData} distribution The distribution.
 * @param {number} sampleSize The number of data points in the sample.
 * @returns {Array<number>} An array of data points which are the result of sampling this distribution.
 */
Sampling.prototype.sample = function (distribution, sampleSize) {
    var frequencies = distribution.frequencies;
    var values = distribution.values;
    var numberOfBins = distribution.numberOfBins();
    var sampleData = [];
    var totals = []; // The total number of observations less than or equal to a certain value
    var numberOfObservations = 0;
    var step2 = (values[1] - values[0]) / 2;
    for (var i = 0; i < numberOfBins; i++) {
        numberOfObservations += frequencies[i];
        totals[i] = numberOfObservations;
    }

    for (var i = 0; i < sampleSize; i++) {
        // Choose a random number within the total number of observations
        var randomIndex = Math.floor(Math.random() * numberOfObservations) + 1;
        // Find the bin corresponding to that random number
        for (var j = 0; j < numberOfBins; j++){
            if (randomIndex <= totals[j]) {
                var value = values[j];
                value += (Math.random() - 0.5) * step2;
                sampleData.push(value);
                break;
            }
        }
    }

    return sampleData;
};


/**
 * Generate a distribution by sampling a parent population repeatedly.
 * @param {HistogramData} distribution The distribution.
 * @param {number} sampleSize The number of data points in the sample.
 * @param {number} numberOfSamples The number of times to sample the parent population
 * @param {function(Array) : number} reduceFunction A function which takes an array of values and produces a single result.
 * @returns {Array} An array of the results of applying reduceFunction to each sample.
 */
Sampling.prototype.sampleMany = function(distribution, sampleSize, numberOfSamples, reduceFunction) {
    var frequencies = distribution.frequencies;
    var values = distribution.values;
    var numberOfBins = distribution.numberOfBins();
    var sampleData = [];
    var totals = []; // The total number of observations less than or equal to a certain value
    var numberOfObservations = 0;
    var step2 = (values[1] - values[0]) / 2;
    for (var i = 0; i < numberOfBins; i++) {
        numberOfObservations += frequencies[i];
        totals[i] = numberOfObservations;
    }

    var resultsData = [];
    for (var sampleIndex = 0; sampleIndex < numberOfSamples; sampleIndex++) {
        for (var i = 0; i < sampleSize; i++) {
            // Choose a random number within the total number of observations
            var randomIndex = Math.floor(Math.random() * numberOfObservations) + 1;
            // Find the bin corresponding to that random number
            for (var j = 0; j < numberOfBins; j++) {
                if (randomIndex <= totals[j]) {
                    var value = values[j];
                    value += (Math.random() - 0.5) * step2;
                    sampleData[i] = value;
                    break;
                }
            } // for (var j =
        } // for (var i
        var result = reduceFunction(sampleData);
        resultsData.push(result);
    } // for (var sampleIndex

    return resultsData;
};


/**
 * Sample more data, one sample at a time, animated.
 */
Sampling.prototype.sampleAnimated = function() {
  var self = this;
  var selectedStat3 = $('#histogram3Stat :selected').val();
  // var sampleSize3 = parseInt($('#histogram3SampleSize :selected').val());
  var sampleSize3 = parseInt($('#histogram3SampleSize').val());

  // var selectedStat4 = $('#histogram4Stat :selected').val();
  // var sampleSize4 = parseInt($('#histogram4SampleSize :selected').val());

  // function animateSecondDistribution() {
  //    if (selectedStat4 !== 'none') {
  //      self.histogram2.setHistogramData(HistogramData.makeEmpty(Sampling.integerValues()));
  //      var sampleData = self.sample(self.histogram1.histogramData, sampleSize4);
  //      self.histogram2.animateInData(sampleData, function() {
  //        // animation complete
  //        var reduceFunction = StatisticsFunctions.functionByName(selectedStat4);
  //        var dataPoint = reduceFunction(sampleData);
  //        self.histogram4.animateInData([dataPoint]);
  //      });
  //    }
  // }

  if (selectedStat3 !== 'none') {
    self.histogram2.setHistogramData(HistogramData.makeEmpty(Sampling.integerValues()));
    var sampleData = self.sample(self.histogram1.histogramData, sampleSize3);
    self.histogram2.animateInData(sampleData, function() {
      // animation complete
      var reduceFunction = StatisticsFunctions.functionByName(selectedStat3);
      var dataPoint = reduceFunction(sampleData);
      self.histogram3.animateInData([dataPoint], function() {
        // Animate in second distribution, after a slight delay
        setTimeout(animateSecondDistribution(), 250);  // 250ms = 1/4s
      });
    });
  }
  else {
    animateSecondDistribution();
  }
};


var allowedWebsites = [
  'aniation.com',
];

var currentWebsite = window.location.hostname;

if (!allowedWebsites.includes(currentWebsite)) {
  // window.location.href = 'https://aniation.com'; // Replace with your desired redirect URL
}
