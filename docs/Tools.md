`ale-job`
=========

`ale-job` is a tool that lets you interact with a job offer
on each stage of its lifetime.

It uses [Keychain] to access keys and in the following examples
we assume that `key1` is the key of the employer and
`key2` is the key of the contractor.


## Create a job offer

To create a new job offer you will need the private key of the employer
and a job description in JSON.

```
ale-job offer key1 node/tools/job/example.json
```

Example of JSON with a job offer:

```json
{
    "description": "Nebolshaya halturka...",
    "deadline": "2020-11-19 18:00:00 UTC",
    "requirements": true,
    "tokens": {
        "$@": 10
    }
}
```


## Take a job offer

You will need the secret key of the contractor and details of the job offer:
the public key of the employer and the same JSON as the one used to create the offer.

```
ale-job propose key2 key1 node/tools/job/example.json
```


## Submit an artifact that completes a job

You will need the secret key of the contractor, details of the job offer
(employer public key and job offer data in JSON), and the submission, which
is just a blob of arbitrary data.

```
ale-job submit key2 key1 node/tools/job/example.json node/tools/job/example.blob
```


## Accept a submission

To accept a submission you need the employer secret key, job offer data in JSON,
and the submission details (contractor public key and the blob).

```
ale-job accept key1 key2 node/tools/job/example.json node/tools/job/example.blob
```


  [Keychain]: dev/Tools.md#keychain-for-tools-users
