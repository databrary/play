'use strict';

module.factory('slotMediaService', [
  '$timeout', 'typeService', function ($timeout, types) {
    // Media

    var Media = function (slot, clock) {
      var media = this;
      this.slot = slot;
      this.clock = clock;

      Object.defineProperties(this, {
        media: {
          value: [],
          writable: false,
          configurable: false
        },

        current: {
          value: [],
          writable: false,
          configurable: false
        },
      });

      this.slot.assets.forEach(function (asset) {
        // TODO: hack until type classes
        asset.container = slot.container;

        if (media.hasDuration(asset)) {
          media.clock.duration = media.clock.duration >= asset.segment[1] ? media.clock.duration : asset.segment[1];
        }
      });

      this.setCurrent(this.slot.assets[0]);

      //

      clock.playFn(this.callbackPlay());
      clock.timeFn(this.callbackTime());
      clock.pauseFn(this.callbackPause());
    };

    Media.prototype.registerMedia = function (media) {
      var that = this;
      this.media.push(media);

      media.$scope.$on('$destroy', function () {
        that.deregisterMedia(media);
      });
    };

    Media.prototype.deregisterMedia = function (media) {
      var i = this.media.indexOf(media);

      if (i > -1) {
        this.media.splice(i, 1);
      }
    };

    Media.prototype.setCurrent = function (asset) {
      this.current[0] = asset;
    };

    Media.prototype.select = function (media) {
      this.current[0] = media.asset;
    };

    // tests

    var getAsset = function (media) {
      return media.element ? media.asset : media;
    };

    Media.prototype.hasPosition = function (media) {
      var asset = getAsset(media);
      return asset.segment;
    };

    Media.prototype.hasDuration = function (media) {
      var asset = getAsset(media);
      return angular.isArray(asset.segment);
    };

    Media.prototype.hasDisplay = function (media) {
      var asset = getAsset(media);
      return ['video', 'image'].indexOf(types.assetMimeArray(asset, true)[0]) > -1;
    };

    Media.prototype.hasTime = function (media) {
      var asset = getAsset(media);
      return ['video'].indexOf(types.assetMimeArray(asset, true)[0]) > -1;
    };

    // callbacks

    var mediaUpdateFn = function (media) {
      media.media.forEach(function (m) {
        if(media.hasTime(m) && media.hasDuration(m)) {
          if (media.clock.position > m.asset.segment[0] && media.clock.position < m.asset.segment[1]) {
            if (m.element.paused) {
              m.element.currentTime = (media.clock.position - m.asset.segment[0]) / 1000;
              m.element.play();
            }
          } else {
            m.element.pause();
          }
        }
      });
    };

    Media.prototype.callbackPlay = function () {
      var that = this;

      return function () {
        mediaUpdateFn(that);
      };
    };

    Media.prototype.callbackTime = function () {
      var that = this;

      return function () {
        mediaUpdateFn(that);
      };
    };

    Media.prototype.callbackPause = function () {
      var that = this;

      return function () {
        that.media.forEach(function (media) {
          if(that.hasTime(media) && that.hasDuration(media)) {
            media.element.pause();
          }
        });
      };
    };

    // Service

    return Media;
  }
]);
