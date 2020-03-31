{--
  Radnyx 2020
--}
module Template
  ( template
  ) where

import Bytebeat

{-- Boilerplate and mixing engine for a song --}
template :: Config -> (String, String, String) -> String
template cfg (ins, vfi, idx) =
  "sample_rate = " ++
  show (samples cfg) ++
  ",\n\
\amplitude = " ++
  show (master cfg) ++
  ",\n\
\\n\
\xx = () => 0.0,\n\
\sn = (t,freq, offset=0) =>\n\
\  sin(t * 6.28 / sample_rate * freq + offset),\n\
\sq = (t, freq, offset=0, [wf=0,wd=0,ww=sn]=[0,0,sn]) =>\n\
\  ((t / sample_rate * freq + offset) % 1 >\n\
\    wd * ww(t, wf) * 0.5 + 0.5) * 2 - 1,\n\
\sw = (t, freq) =>\n\
\    ((t / sample_rate * freq) % 1) * 2 - 1,\n\
\ns = (t, freq) => {\n\
\  const nf = floor((t & 262143) * freq * 400000 / sample_rate / sample_rate);\n\
\  return ((16384 * sin(nf * nf)) & 128) / 64.0 - 1;\n\
\},\n\
\\n\
\window.song = (t >= 1) ? window.song :\n\
\{\n\
\ins:" ++
  ins ++
  ",\n\
\vfi:" ++
  vfi ++
  ",\n\
\idx:" ++
  idx ++
  ",\n\
\rvb:" ++
  rvb ++
  ",\n\
\mix:" ++
  show (mix cfg) ++
  "\n\
\},\n\
\\n\
\layer = (t, freq, waves, p2) => {\n\
\  if (waves.concat === undefined) return waves(t, freq);\n\
\  let mix = 0.0;\n\
\  if (waves[0]) mix += sn(t, freq) * 1.0;\n\
\  if (waves[1]) mix += sq(t, freq, 0, p2) * 0.8;\n\
\  if (waves[2]) mix += sw(t, freq) * 0.8;\n\
\  return mix;\n\
\},\n\
\\n\
\harm = (t, count, vol, freq, waves, [p1freq, p1depth, p1wav], p2)  => {\n\
\  if (freq == 0.0) return 0.0;\n\
\  let mix = 0.0;\n\
\  mix = vol * layer(t, freq, waves, p2);\n\
\  for (let i = 2; i <= count; i++) {\n\
\    vol *= 0.75;\n\
\    mix += (p1wav(t, p1freq, i) * p1depth) * vol * layer(t, freq * i, waves, p2);\n\
\  }\n\
\  return mix;\n\
\},\n\
\\n\
\generate = (t, reverb) => {\n\
\  if (t < 0) return 0.0;\n\
\  const time = t / sample_rate * 12.93 % window.song.idx[0].length;\n\
\  const step = floor(time);\n\
\  const vprog = time - step;\n\
\  const fprog = floor(vprog * 8) / 8;\n\
\\n\
\  let mix = 0.0;\n\
\  for (let i = 0; i < window.song.idx.length; i++) {\n\
\    if (reverb !== undefined && !reverb[i]) continue;\n\
\    const [vol, freq, instr] = window.song.vfi[i][window.song.idx[i][step]];\n\
\    const [waves, h, [p0freq=0, p0depth=0, p0wav=xx], p1, p2] = window.song.ins[instr];\n\
\    const v = vol.concat === undefined\n\
\      ? vol\n\
\      : vol[1] * vprog + vol[0] * (1 - vprog);\n\
\    const f = freq.concat === undefined\n\
\      ? freq\n\
\      : freq[1] * fprog + freq[0] * (1 - fprog);\n\
\    mix += window.song.mix[i] * harm(t, h, v, f + p0wav(t, p0freq) * p0depth / t * sample_rate, waves, p1, p2);\n\
\  }\n\
\  return mix;\n\
\},\n\
\\n\
\128*((generate(t) + 0.25 * generate(t - sample_rate * 0.2, window.song.rvb)) * amplitude) + 127\n\
\\n"
  where
    rvb =
      show $
      (\x ->
         if x
           then 1 :: Int
           else 0) <$>
      reverb cfg
