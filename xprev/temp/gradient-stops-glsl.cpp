

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    // these should be uniforms
    vec2 gradient_start_pos = vec2(0.0, 0.0); // top-left
    vec2 gradient_end_pos = vec2(1.0, 1.0); // bottom-right

    // define colors and stops
    const int num_stops = 4;
    float stops[32];
    vec4 colors[32];
    stops[0] = 0.0;
    stops[1] = 0.2;
    stops[2] = 0.6;
    stops[3] = 1.0;
    colors[0] = vec4(1.0, 0.0, 0.0, 1.0);
    colors[1] = vec4(0.0, 0.0, 0.0, 1.0);
    colors[2] = vec4(1.0, 1.0, 0.0, 1.0);
    colors[3] = vec4(0.0, 1.0, 1.0, 1.0);


	vec2 uv = (fragCoord.xy / iResolution.xy);
    uv.y = (uv.y - 1.0) * -1.0;


    float alpha = 0.; // this is the angle of the gradient in rad

    float gradient_startpos_rotated_x = gradient_start_pos.x * cos(-alpha) - gradient_start_pos.y * sin(-alpha);
    float gradient_endpos_rotated_x = gradient_end_pos.x * cos(-alpha) - gradient_end_pos.y * sin(-alpha);
    float len = gradient_endpos_rotated_x - gradient_startpos_rotated_x;
    float x_loc_rotated = uv.x * cos(-alpha) - uv.y * sin(-alpha);

    if (num_stops == 1) {
        fragColor = colors[0];
    } else if (num_stops > 1) {
        fragColor = mix(colors[0], colors[1], smoothstep(
            gradient_startpos_rotated_x + stops[0] * len,
            gradient_startpos_rotated_x + stops[1] * len,
            x_loc_rotated
        ));
        for (int i = 1; i < 32 - 1; i++) {
            if (i < num_stops - 1) {
                fragColor = mix(fragColor, colors[i + 1], smoothstep(
                    gradient_startpos_rotated_x + stops[i] * len,
                    gradient_startpos_rotated_x + stops[i + 1] * len,
                    x_loc_rotated
                ));
            } else { break; }
        }
    }
}