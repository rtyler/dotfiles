#!/bin/bash
echo $(($(cat /sys/devices/platform/thinkpad_hwmon/temp10_input) / 1000))
