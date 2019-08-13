#!/bin/bash
# Spotify song recorder.
# For Ubuntu users: apt install sox libsox-fmt-mp3 id3
# TODO: allow local playback with command line option
# TODO: record into playlist directory instead of album

LAST_FILENAME=""

function get_track_info() {
    dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata' 2> /dev/null
}

function get_track_id() {
    echo "$@" | sed -re 's/.*string "mpris:trackid" variant string "spotify:([^"]+)".*/\1/'
}

function get_artist_name() {
    echo "$@" | sed -re 's/.*string "xesam:artist" variant array \[ string "([^"]+)".*/\1/'
}

function get_track_name() {
    echo "$@" | sed -re 's/.*string "xesam:title" variant string "([^"]+)".*/\1/'
}

function get_track_number() {
    echo "$@" | sed -re 's/.*string "xesam:trackNumber" variant int32 ([0-9]+).*/\1/'
}

function get_album_name() {
    echo "$@" | sed -re 's/.*string "xesam:album" variant string "([^"]+)".*/\1/'
}

function get_album_disc() {
    echo "$@" | sed -re 's/.*string "xesam:discNumber" variant int32 ([0-9]+).*/\1/'
}

function get_album_artist() {
    echo "$@" | sed -re 's/.*string "xesam:albumArtist" variant array \[ string "([^"]+)".*/\1/'
}

function wait_for_next_track() {
    local TRACKID=$(get_track_id $(get_track_info))
    local TMP="${TRACKID}"
    while [ "$TMP" == "${TRACKID}" ]; do
          TMP=$(get_track_id $(get_track_info))
          sleep 0.1
    done
}

function record_track() {
    local TRACKINFO=$(get_track_info)
    if [ -z "${TRACKINFO}" ]; then
        echo "Failed to fetch track info. Is Spotify running?"
        return 1
    fi
    
    local TRACKID=$(get_track_id ${TRACKINFO})

    case "${TRACKID}" in
        track:*)
            local TRACK_ARTIST=$(get_artist_name ${TRACKINFO})
            local TRACK_NAME=$(get_track_name ${TRACKINFO})
            local TRACK_NUMBER=$(get_track_number ${TRACKINFO})
            
            local ALBUM_ARTIST=$(get_album_artist ${TRACKINFO})
            local ALBUM_DISC=$(get_album_disc ${TRACKINFO})
            local ALBUM_NAME=$(get_album_name ${TRACKINFO})
            
            local OUTPUT_DIR="${HOME}/Music/${ALBUM_ARTIST////} - ${ALBUM_NAME////} - ${ALBUM_DISC////}"
            local OUTPUT_FILE="${TRACK_NUMBER////} - ${TRACK_ARTIST////} - ${TRACK_NAME////}.mp3"
            local FILENAME="${OUTPUT_DIR}/${OUTPUT_FILE}"
            LAST_FILENAME="${FILENAME}"
            
            echo "Recording ${TRACK_NAME} by ${TRACK_ARTIST}"
            
            mkdir -p "${OUTPUT_DIR}"
            parec -d spotify-recorder.monitor 2>/dev/null | sox -t raw -r 44100 -b 16 -L -e signed -c 2 - -C192 "${FILENAME}" &
            LAME_PID=$!

            wait_for_next_track

            kill $LAME_PID
            (sleep 5; id3 -t "${TRACK_NAME}" -T "${TRACK_NUMBER}" -A "${ALBUM_NAME}" -a "${TRACK_ARTIST}" "${FILENAME}" > /dev/null) &
            ;;
        ad:*)
            LAST_FILENAME=""
            echo Waiting for advertisement to end.
            wait_for_next_track
            move_spotify_input
    esac
}

function find_spotify_input() {
    local LINE
    local LAST_INDEX=
    pacmd list-sink-inputs | grep -E 'application.process.binary = "spotify"|index:' | \
        while read LINE; do
            case "${LINE}" in
                index:*)
                    LASTINDEX=$(echo ${LINE} | sed -re 's/index: (.*)/\1/') ;;
                application.process.binary*)
                    echo $LASTINDEX
                    return 0 ;;
            esac
        done
    return 1
}

function cleanup() {
    echo "Removing incomplete recording ${LAST_FILENAME}"
    rm -f "${LAST_FILENAME}"
    echo "Moving Spotify ($SPOTIFY_INPUT) back to default sink."
    pactl move-sink-input $SPOTIFY_INPUT @DEFAULT_SINK@
    echo "Removing monitor and mixer sinks."
    pactl unload-module module-null-sink
    pactl unload-module module-combine-sink
    exit 0
}

function create_sink() {
    if !( pactl load-module module-null-sink sink_name=spotify-recorder ); then
        echo "Failed to create PulseAudio sink, giving up."
        exit 1
    fi
    if !( pactl load-module module-combine-sink sink_name=spotify-mixer slaves=spotify-recorder,${DEFAULT_SINK} ); then
        echo "Failed to create PulseAudio sink, giving up."
        exit 1
    fi
}

function move_spotify_input() {
    SPOTIFY_INPUT=$(find_spotify_input)
    if [ -z ${SPOTIFY_INPUT} ]; then
        echo "Failed to find Spotify input. Is Spotify running?"
        exit 1
    fi
    
    if !( pactl move-sink-input $SPOTIFY_INPUT spotify-mixer ); then
        echo "Failed to move Spotify to capture sink, giving up."
        exit 1
    fi
}

DEFAULT_SINK=$(pactl info | egrep "^Default Sink" | cut -d: -f2 | cut -c 2-)
create_sink
move_spotify_input
trap cleanup SIGINT
echo Waiting for first track to end before recording starts
wait_for_next_track
echo Starting recording, press Ctrl-C to stop.
while true; do
    record_track || break
done
cleanup
