output_fname="cmr10.tfm"
if [[ -a "${output_fname}" ]]; then
    echo "File '${output_fname}' exists; not overwriting."
    exit 1
else
    curl --location http://mirrors.ctan.org/fonts/cm/tfm/cmr10.tfm -o "${output_fname}"
fi
