# github.com/databrary/databrary

Databrary 2.0 API

## Routes

<details>
<summary>`/`</summary>

- **/**
	- _GET_
		- [main.main.func1](/databrary.go#L117)

</details>
<details>
<summary>`/api/autocomplete-affil`</summary>

- **/api**
	- **/autocomplete-affil**
		- _GET_
			- [AutoCompleteAffil](/routes/api.go#L66)

</details>
<details>
<summary>`/api/loggedin`</summary>

- **/api**
	- **/loggedin**
		- _GET_
			- [IsLoggedInEndpoint](/routes/user.go#L187)

</details>
<details>
<summary>`/api/report-error`</summary>

- **/api**
	- **/report-error**
		- _POST_
			- [ReportError](/routes/api.go#L126)

</details>
<details>
<summary>`/api/site-stats`</summary>

- **/api**
	- **/site-stats**
		- _GET_
			- [GetSiteStats](/routes/api.go#L138)

</details>
<details>
<summary>`/api/user/check-token`</summary>

- **/api**
	- **/user**
		- **/check-token**
			- _POST_
				- [CheckTokenExpiryEndpoint](/routes/user.go#L290)

</details>
<details>
<summary>`/api/user/exists`</summary>

- **/api**
	- **/user**
		- **/exists**
			- _GET_
				- [UserExists](/routes/user.go#L476)

</details>
<details>
<summary>`/api/user/login`</summary>

- **/api**
	- **/user**
		- **/login**
			- _POST_
				- [PostLogin](/routes/user.go#L28)
			- _GET_
				- [GetLogin](/routes/static.go#L12)

</details>
<details>
<summary>`/api/user/logout`</summary>

- **/api**
	- **/user**
		- **/logout**
			- _POST_
				- [PostLogOut](/routes/user.go#L117)

</details>
<details>
<summary>`/api/user/profile`</summary>

- **/api**
	- **/user**
		- **/profile**
			- **/**
				- _GET_
					- [GetProfile](/routes/user.go#L648)
				- _PATCH_
					- [PatchProfile](/routes/user.go#L680)

</details>
<details>
<summary>`/api/user/register`</summary>

- **/api**
	- **/user**
		- **/register**
			- _POST_
				- [Register](/routes/user.go#L522)

</details>
<details>
<summary>`/api/user/reset-password/email`</summary>

- **/api**
	- **/user**
		- **/reset-password/email**
			- _POST_
				- [ResetPasswordEmail](/routes/user.go#L208)

</details>
<details>
<summary>`/api/user/reset-password/token`</summary>

- **/api**
	- **/user**
		- **/reset-password/token**
			- _POST_
				- [ResetPasswordToken](/routes/user.go#L353)

</details>
<details>
<summary>`/api/user/volume/*`</summary>

- **/api**
	- **/user**
		- **/volume**
			- **/***
				- **/**
					- _GET_
						- [GetUserVolumes](/routes/volume.go#L25)

</details>
<details>
<summary>`/public`</summary>

- **/public**
	- _GET_
		- [(Handler).ServeHTTP-fm](https:///usr/local/go/src/net/http/h2_bundle.go#L4331)

</details>
<details>
<summary>`/public/*`</summary>

- **/public/***
	- _GET_
		- [(*Mux).FileServer.func1](https://github.com/pressly/chi/mux.go#L317)

</details>

Total # of routes: 16

