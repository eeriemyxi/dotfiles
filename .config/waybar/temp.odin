// Build with: odin build temp.odin -file

package temp

import "core:fmt"
import "core:strconv"
import "core:math"
import os "core:os/os2"

PATH :: "/sys/class/hwmon/hwmon2/temp1_input"
DECIMAL_PLACES :: 1

main :: proc() {
    temp_raw, err := os.read_entire_file_from_path(PATH, context.allocator)
    if err != nil {
        panic(fmt.tprintf("Error reading file %s", PATH))
    }

    temp := strconv.atoi(transmute(string)temp_raw)
    n := math.pow10(cast(f32) DECIMAL_PLACES)
    rounded_temp := math.floor((cast(f32)temp / 1000) * n) / n
    places := 0 if math.floor(rounded_temp) == rounded_temp else DECIMAL_PLACES

    fmt.printf(fmt.tprintf("%%.%df", places), rounded_temp)
}
