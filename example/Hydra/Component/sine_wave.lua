local MARGIN = 10
local AMPLITUDE, WAVE_WIDTH = 30, 600
local GAP_BETWEEN_WAVES = 25
local NUM_WAVES = 7
local FONT_SIZE, FONT_FUDGE = 20, 5

local WAVE_STYLE = ' stroke="#000" stroke-width="1.5" fill="none"'

local img_wd = 2 * MARGIN + WAVE_WIDTH
local img_ht = 2 * MARGIN + NUM_WAVES * AMPLITUDE * 2 +
               (NUM_WAVES - 1) * GAP_BETWEEN_WAVES

local PI = math.asin(1) * 2
local XD = PI / 12
local SQRT2 = math.sqrt(2)
local Y1 = (2 * SQRT2) / 7 - 1 / 7
local Y2 = (4 * SQRT2) / 7 - 2 / 7
local Y3 = SQRT2 / 2
local Y4 = (3 * SQRT2) / 7 + 2 / 7

function sine_wave_path (x, y, width, amp, num_half_waves)
    local xmul = width / (num_half_waves * PI)
    local xd = XD * xmul
    local path = 'M' .. coords(x, y)
    for _ = 1, num_half_waves do
        path = path .. ' C' .. coords(x + xd,   y + amp * Y1)
                    ..  ' ' .. coords(x + 2*xd, y + amp * Y2)
                    ..  ' ' .. coords(x + 3*xd, y + amp * Y3)
                    .. ' C' .. coords(x + 4*xd, y + amp * Y4)
                    ..  ' ' .. coords(x + 5*xd, y + amp)
                    ..  ' ' .. coords(x + 6*xd, y + amp)
                    .. ' C' .. coords(x + 7*xd, y + amp)
                    ..  ' ' .. coords(x + 8*xd, y + amp * Y4)
                    ..  ' ' .. coords(x + 9*xd, y + amp * Y3)
                    .. ' C' .. coords(x + 10*xd, y + amp * Y2)
                    ..  ' ' .. coords(x + 11*xd, y + amp * Y1)
                    ..  ' ' .. coords(x + 12*xd, y)
        x = x + width / num_half_waves
        amp = amp * -1  -- flip over vertically every half wave
    end
    return path
end

function coords (x, y)
    return string.format('%g,%g', x, y)
end

local fh = assert(io.open("Harmonic_partials_on_strings.svg", "wb"))
fh:write('<?xml version="1.0" encoding="UTF-8"?>\n' ..
         '<svg version="1.0" width="', img_wd, '" height="', img_ht,
         '" xmlns="http://www.w3.org/2000/svg">\n')

local y_origin = MARGIN + AMPLITUDE
local wave_labels, pick_circles = '', ''
for n = 1, NUM_WAVES do
    local amp = AMPLITUDE - (n - 1) * AMPLITUDE * 0.1
    fh:write(' <path', WAVE_STYLE, ' d="',
             sine_wave_path(MARGIN, y_origin, WAVE_WIDTH, amp, n), ' ',
             sine_wave_path(MARGIN, y_origin, WAVE_WIDTH, -amp, n),
             '"/>\n')
    if n > 1 then
        local pick_x = MARGIN + WAVE_WIDTH / n
        local pick_r = AMPLITUDE * 0.2
        pick_circles = pick_circles .. '  <circle cx="' .. pick_x ..
                       '" cy="' .. y_origin + pick_r ..
                       '" r="' .. pick_r .. '"/>\n'
        wave_labels = wave_labels .. '  <text x="' .. pick_x ..
                      '" y="' .. y_origin - FONT_SIZE + 2 * FONT_FUDGE ..
                      '">1/' .. n .. '</text>\n'
    end
    y_origin = y_origin + 2 * AMPLITUDE + GAP_BETWEEN_WAVES
end

fh:write(' <g stroke="#000" fill="#bbb">\n', pick_circles, ' </g>\n',
         ' <g font-family="Bitstream Vera Sans" font-size="', FONT_SIZE, 'px"',
         ' text-anchor="middle">\n',
         '  <text x="', MARGIN + FONT_FUDGE,
         '" y="', MARGIN + AMPLITUDE - FONT_SIZE + 2 * FONT_FUDGE,
         '">0</text>\n',
         '  <text x="', MARGIN + WAVE_WIDTH - FONT_FUDGE,
         '" y="', MARGIN + AMPLITUDE - FONT_SIZE + 2 * FONT_FUDGE,
         '">1</text>\n',
         wave_labels,
         ' </g>\n',
         '</svg>\n')