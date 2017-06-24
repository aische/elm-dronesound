function Osc (soundplayer) {
	this.soundplayer = soundplayer;
	this.osc = this.soundplayer.audiocontext.createOscillator  ();
    this.osc.type = "sawtooth";
	this.osc.frequency.value = 500;
    this.filter = this.soundplayer.audiocontext.createBiquadFilter ();
    this.filter.type = "lowpass";
    this.filter.frequency.value = 500;
    this.pan = this.soundplayer.audiocontext.createStereoPanner();
    this.gain = this.soundplayer.audiocontext.createGain ();
    this.gain.gain.value = 0.0;
    
    this.osc.connect (this.filter);
    this.filter.connect (this.pan);
    this.pan.connect (this.gain);
    this.gain.connect (this.soundplayer.destination);
    this.osc.start(this.soundplayer.audiocontext.currentTime + 0.1);
}

Osc.prototype.set = function (freq, volume, panning, cutoff) {
    var delta = 0.05;
    panning = panning || 0;
	this.gain.gain.linearRampToValueAtTime(volume, this.soundplayer.audiocontext.currentTime + delta);
    this.osc.frequency.linearRampToValueAtTime(freq, this.soundplayer.audiocontext.currentTime + delta);
    this.pan.pan.linearRampToValueAtTime(panning, this.soundplayer.audiocontext.currentTime + delta);
	this.filter.frequency.linearRampToValueAtTime(Math.min(10000, freq*cutoff), this.soundplayer.audiocontext.currentTime + delta);
}

function SoundPlayer () {
    this.audiocontext = 'AudioContext' in window ? new AudioContext () : new webkitAudioContext ();
    this.destination = this.audiocontext.createGain ();
    this.destination.connect (this.audiocontext.destination);
    this.destination.gain.value = 0.0;
    this.oscs = {};
}

SoundPlayer.midicps = function (note) {
    var y = Math.pow (2, note / 12) * 8.1757989156437;
    return y;
};

SoundPlayer.prototype.setOscs = function (list) {
    var i, j, args, len;
    for (i=0; i<list.length; i++) {
        args = list[i];
        this.setOsc(args[0], args[1].note, args[1].amp, args[1].pan, args[1].cutoff);
    }
    len = Object.keys(this.oscs).length;
    if (len > i) {
        for (j=i; j<len; j++) {
            this.removeOsc(j);
        }
    }
}
SoundPlayer.prototype.setOsc = function (key, note, volume, panning, cutoff) {
    var freq = SoundPlayer.midicps(note);
    if (this.oscs[key]) {
        this.oscs[key].set(freq, volume, panning, cutoff);
    } else {
        this.oscs[key] = new Osc(this);
        this.oscs[key].set(freq, volume, panning, cutoff);
    }
}

SoundPlayer.prototype.removeOsc = function (key) {
   if (this.oscs[key]) {
        this.oscs[key].osc.stop();
        delete this.oscs[key];
    }
}
