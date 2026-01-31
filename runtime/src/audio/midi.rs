//! MIDI file synthesis using a SoundFont (MIDI → PCM).
//!
//! Used when playing `.mid` files via `_SNDOPEN` / `_SNDPLAYFILE` with a
//! soundfont set by `_MIDISOUNDBANK`. Loads the SoundFont and MIDI file,
//! runs the sequencer, and renders stereo float PCM suitable for rodio.
//!
//! QB64pe uses TinySoundFont (tsf) + foo_midi; we use **rustysynth**
//! (SoundFont + `MidiFileSequencer`).

use std::fs::File;
use std::io::BufReader;
use std::sync::Arc;

use rustysynth::{MidiFile, MidiFileSequencer, SoundFont, Synthesizer, SynthesizerSettings};

/// Sample rate used for MIDI synthesis (matches common soundfonts and rodio).
pub const MIDI_SAMPLE_RATE: i32 = 44100;

/// Renders a MIDI file to stereo float PCM using the given SoundFont file.
///
/// # Arguments
///
/// * `midi_path` - Path to the `.mid` file
/// * `soundfont_path` - Path to the `.sf2` / `.sf3` soundfont file
///
/// # Returns
///
/// `Ok((samples, sample_rate))` where `samples` is stereo interleaved
/// (L, R, L, R, ...) f32 in [-1, 1], and `sample_rate` is 44100.
///
/// # Errors
///
/// Returns an error string if the soundfont or MIDI file cannot be loaded,
/// or if the synthesizer cannot be created.
pub fn render_midi_to_pcm(
    midi_path: &str,
    soundfont_path: &str,
) -> Result<(Vec<f32>, u32), String> {
    let mut sf_reader =
        BufReader::new(File::open(soundfont_path).map_err(|e| format!("SoundFont open: {}", e))?);
    let sound_font =
        Arc::new(SoundFont::new(&mut sf_reader).map_err(|e| format!("SoundFont load: {:?}", e))?);

    let mut midi_reader =
        BufReader::new(File::open(midi_path).map_err(|e| format!("MIDI file open: {}", e))?);
    let midi_file =
        Arc::new(MidiFile::new(&mut midi_reader).map_err(|e| format!("MIDI file load: {:?}", e))?);

    let settings = SynthesizerSettings::new(MIDI_SAMPLE_RATE);
    let synthesizer =
        Synthesizer::new(&sound_font, &settings).map_err(|e| format!("Synthesizer: {:?}", e))?;
    let block_size = synthesizer.get_block_size();

    let mut sequencer = MidiFileSequencer::new(synthesizer);
    sequencer.play(&midi_file, false);

    let mut left_buf = vec![0f32; block_size];
    let mut right_buf = vec![0f32; block_size];
    let mut samples = Vec::with_capacity(block_size * 2 * 1024); // rough initial capacity

    while !sequencer.end_of_sequence() {
        sequencer.render(&mut left_buf, &mut right_buf);
        for i in 0..block_size {
            samples.push(left_buf[i]);
            samples.push(right_buf[i]);
        }
    }

    Ok((samples, MIDI_SAMPLE_RATE as u32))
}
